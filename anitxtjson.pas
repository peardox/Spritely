unit AniTxtJson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, CastleDownload;

type
  TAniLine = record
    Success: Boolean;
    FromFrame: Integer;
    ToFrame: Integer;
    Action: String;
  end;

function AniTxtToJson(const InFile: String): TJsonNode;
function ParseText(const S: String): TAniLine;
function ParseText_FormatA(const S: String): TAniLine;
function ParseText_FormatB(const S: String): TAniLine;

implementation

const
  CHECK_TAB      = $09;
  CHECK_DIGIT_0  = $30;
  CHECK_DIGIT_9  = $39;
  CHECK_COLON    = $3A;
  CHECK_UCASE_A  = $41;
  CHECK_UCASE_Z  = $5A;
  CHECK_LCASE_A  = $61;
  CHECK_LCASE_Z  = $7A;

{$inline on}
function isDigit(c: BYTE): Boolean; inline;
begin
  Result := (((c >= CHECK_DIGIT_0) and (c <= CHECK_DIGIT_9)));
end;

function isAlpha(c: BYTE): Boolean; inline;
begin
  Result := (((c >= CHECK_UCASE_A) and (c <= CHECK_UCASE_Z)) or
             ((c >= CHECK_LCASE_A) and (c <= CHECK_LCASE_Z)));
end;
{$inline off}

// <Action><Tab(s)><Start><End>
function ParseText_FormatC(const S: String): TAniLine;
var
  p: PChar;
  l: Integer;
  i: Integer;
  stage: Integer;
  TmpFrom: String;
  TmpTo: String;
begin
  stage := 0;
  TmpFrom := EmptyStr;
  TmpTo := EmptyStr;
  Result := Default(TAniLine);

  // Get a pointer to the start of the string
  p := PChar(S);
  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 0 to l - 1 do
    begin

      if (stage = 0) and (ord(p^) = CHECK_TAB) then
        Inc(stage)
      else if (stage > 0) and isDigit(ord(p^)) then
        begin
          case stage of
            1:
              begin
                TmpFrom += p^;
              end;
            2:
              begin
                TmpTo += p^;
              end;
            end;
          end
        else
          begin
            if (stage = 0) then
              Result.Action += p^;
            if (stage = 1) and (Length(TmpFrom) > 0) and not(isDigit(ord(p^))) then
              Inc(stage);
            if (stage = 2) and (Length(TmpTo) > 0) and not(isDigit(ord(p^))) then
              Break;
          end;
      Inc(p);
    end;

  if (stage = 2) then
    begin
      Result.Success := True;
      Result.FromFrame := StrToIntDef(TmpFrom.Trim, 0);
      Result.ToFrame := StrToIntDef(TmpTo.Trim, 0);
      Result.Action := Result.Action.Trim;
    end;
end;

// <Action>:<Start><End>
function ParseText_FormatB(const S: String): TAniLine;
var
  p: PChar;
  l: Integer;
  i: Integer;
  stage: Integer;
  TmpFrom: String;
  TmpTo: String;
begin
  stage := 0;
  TmpFrom := EmptyStr;
  TmpTo := EmptyStr;
  Result := Default(TAniLine);

  // Get a pointer to the start of the string
  p := PChar(S);
  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 0 to l - 1 do
    begin

      if (stage = 0) and (ord(p^) = CHECK_COLON) then
        Inc(stage)
      else if (stage > 0) and isDigit(ord(p^)) then
        begin
          case stage of
            1:
              begin
                TmpFrom += p^;
              end;
            2:
              begin
                TmpTo += p^;
              end;
            end;
          end
        else
          begin
            if (stage = 0) then
              Result.Action += p^;
            if (stage = 1) and (Length(TmpFrom) > 0) and not(isDigit(ord(p^))) then
              Inc(stage);
            if (stage = 2) and (Length(TmpTo) > 0) and not(isDigit(ord(p^))) then
              Break;
          end;
      Inc(p);
    end;

  if (stage = 2) then
    begin
      Result.Success := True;
      Result.FromFrame := StrToIntDef(TmpFrom.Trim, 0);
      Result.ToFrame := StrToIntDef(TmpTo.Trim, 0);
      Result.Action := Result.Action.Trim;
    end;
end;

// <Start><End><Action>
function ParseText_FormatA(const S: String): TAniLine;
var
  p: PChar;
  l: Integer;
  i: Integer;
  stage: Integer;
  TmpFrom: String;
  TmpTo: String;
begin
  stage := 0;
  TmpFrom := EmptyStr;
  TmpTo := EmptyStr;
  Result := Default(TAniLine);

  // Get a pointer to the start of the string
  p := PChar(S);
  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 0 to l - 1 do
    begin

      if (stage < 2) and isDigit(ord(p^)) then
        begin
          case stage of
            0:
              begin
                TmpFrom += p^;
              end;
            1:
              begin
                TmpTo += p^;
              end;
            end;
          end
        else
          begin
            if (stage = 0) and (Length(TmpFrom) > 0) and not(isDigit(ord(p^))) then
              Inc(stage);
            if (stage = 1) and (Length(TmpTo) > 0) and not(isDigit(ord(p^))) then
              Inc(stage);
            if (stage = 2) then
              Result.Action += p^;
          end;
      Inc(p);
    end;

  if (stage = 2) then
    begin
      Result.Success := True;
      Result.FromFrame := StrToIntDef(TmpFrom.Trim, 0);
      Result.ToFrame := StrToIntDef(TmpTo.Trim, 0);
      Result.Action := Result.Action.Trim;
    end;
end;

function ParseText(const S: String): TAniLine;
var
  p: PChar;
  l: Integer;
  i: Integer;
begin
  Result := Default(TAniLine);

  // Get a pointer to the start of the string
  p := PChar(S);
  // Length of string
  l := Length(S);

  // Loop over the string
  for i := 0 to l - 1 do
    begin
      if isDigit(ord(p^)) then
        begin
          Result := ParseText_FormatA(S);
          Break;
        end;
      if isAlpha(ord(p^)) then
        begin
          Result := ParseText_FormatB(S);
          if not(Result.Success) then
            Result := ParseText_FormatC(S);
          Break;
        end;
      Inc(p);
    end;
end;

function AniTxtToJson(const InFile: String): TJsonNode;
var
  F: TTextReader;
  I: Integer;
  S: string;
  L: TAniLine;
  Json: TJsonNode;
  JArray: TJsonNode;
  JObject: TJsonNode;
begin
  if not(FileExists(InFile)) then
    begin
      if IsConsole then
        WriteLn('File not found');
      Exit(nil);
    end;
  Json := TJsonNode.Create;
  JArray := Json.Add('Animations', nkObject);
  JArray.Kind := nkArray;

  I := 0;
  F := nil;
  try
    F := TTextReader.Create(InFile);
    try
      while not F.Eof do
        begin
          Inc(I);
          S := F.ReadLn;
          S := S.Trim;
          if Length(S) = 0 then
            continue;
          L := ParseText(S);
          if not(L.Success) then
            begin
              JSon.Free;
              JSon := nil;
              Break;
            end;
          JObject := JArray.Add('', nkObject);
          JObject.Add('FromFrame', L.FromFrame);
          JObject.Add('ToFrame', L.ToFrame);
          JObject.Add('Action', L.Action);
        end;
    except
      on E : Exception do
        begin
          if IsConsole then
            WriteLn('Error (' + E.ClassName + ') : ' + E.Message);
        end;
    end;
  finally
    if F <> nil then
      F.Free;
  end;

  Result := Json;
end;

end.

