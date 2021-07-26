unit CastlePageControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors,
  CastleControl, CastleControls, CastleColors, CastleUIControls;

type
  { TCastleTabSheet }

  TCastleTabSheet = class(TCastleControl)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; ACaption: String; AURL: String);
  private
    fRect: TCastleRectangleControl;
    fImage: TCastleImageControl;
    fCaption: String;
    procedure DoMouseEnterTab(const Sender: TCastleUserInterface);
    procedure DoMouseLeaveTab(const Sender: TCastleUserInterface);
  end;
  TCastleTabSheetArray = Array of TCastleTabSheet;

  { TCastlePageControl }
  TCastlePageControl = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
  private
    fTabSection: TCastleUserInterface;
    fTabHGroup: TCastleHorizontalGroup;
    fTabVGroup: TCastleVerticalGroup;
    fTabs: TCastleTabSheetArray;
    fTabCurrent: TCastleRectangleControl;
    fTabActive: TCastleTabSheet;
    fImgMargin: Single;
    fTabCaption: TCastleLabel;
    fTabSectionHeight: Single;
    fTabCaptionHeight: Single;
    fTabActiveColor: TCastleColor;
    fTabHoverColor: TCastleColor;
    fTabInActiveColor: TCastleColor;
    procedure SetTabActive(const AValue: TCastleTabSheet);
    procedure SetTabActiveColor(const Value: TCastleColor);
    procedure SetTabHoverColor(const Value: TCastleColor);
    procedure SetTabInActiveColor(const Value: TCastleColor);
  public
    procedure AddTab(const ACaption: String; const AURL: String);
    property TabHGroup: TCastleHorizontalGroup read fTabHGroup write fTabHGroup;
    property TabActive: TCastleTabSheet read fTabActive write SetTabActive;
    property TabActiveColor: TCastleColor read fTabActiveColor write SetTabActiveColor;
    property TabHoverColor: TCastleColor read fTabHoverColor write SetTabHoverColor;
    property TabInActiveColor: TCastleColor read fTabInActiveColor write SetTabInActiveColor;
  published
    property ImgMargin: Single read fImgMargin write fImgMargin;
    property TabSectionHeight: Single read fTabSectionHeight write fTabSectionHeight;
    property TabCaptionHeight: Single read fTabCaptionHeight write fTabCaptionHeight;
  end;

implementation

{ TCastleTabSheet }
constructor TCastleTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

constructor TCastleTabSheet.Create(AOwner: TComponent; ACaption: String; AURL: String);
begin
  Create(AOwner);

  fCaption := ACaption;

  with AOwner as TCastlePageControl do
    begin
      fRect := TCastleRectangleControl.Create(TabHGroup);
      fRect.Color := TabActiveColor;
      fRect.Height := TabSectionHeight;
      fRect.Width := TabSectionHeight;
      fRect.OnInternalMouseEnter := @DoMouseEnterTab;
      fRect.OnInternalMouseLeave := @DoMouseLeaveTab;
      TabHGroup.InsertFront(fRect);

      fImage := TCastleImageControl.Create(fRect);
      fImage.Height := TabSectionHeight - ImgMargin;
      fImage.Width := TabSectionHeight - ImgMargin;
      fImage.Left := (ImgMargin / 2) - fRect.Border.AllSides;
      fImage.Bottom := (ImgMargin / 2) - fRect.Border.AllSides;
      fImage.URL := AURL;
      fImage.Stretch := True;
      fRect.InsertFront(fImage);

    end;

end;

procedure TCastleTabSheet.DoMouseEnterTab(const Sender: TCastleUserInterface);
begin
//  if not(TCastlePageControl(Parent).TabActive = Sender) then
    begin
      TCastleRectangleControl(Sender).BorderColor := Red;
      TCastleRectangleControl(Sender).Border.AllSides := 1;
    end;
end;

procedure TCastleTabSheet.DoMouseLeaveTab(const Sender: TCastleUserInterface);
begin
//  if not(TCastlePageControl(Parent).TabActive = Sender) then
    begin
      TCastleRectangleControl(Sender).BorderColor := Vector4(0.8,0.8,0.8,1.0);
      TCastleRectangleControl(Sender).Border.AllSides := 0;
    end;
end;

{ TCastlePageControl }
constructor TCastlePageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := TCastleUserInterface(AOwner).Width;
  Height := TCastleUserInterface(AOwner).Height;

  fTabSectionHeight := 40;
  fTabCaptionHeight := 24;

  fTabSection := TCastleUserInterface.Create(Self);
  fTabSection.Height := fTabSectionHeight + fTabCaptionHeight;
  fTabSection.Width := Width;
  Self.InsertFront(fTabSection);

  fTabVGroup := TCastleVerticalGroup.Create(fTabSection);
  fTabVGroup.Height := fTabSectionHeight + fTabCaptionHeight;
  fTabVGroup.Width := Width;
  fTabSection.InsertFront(fTabVGroup);

  fTabHGroup := TCastleHorizontalGroup.Create(fTabVGroup);
  fTabHGroup.Height := fTabSectionHeight;
  fTabHGroup.Width := Width;
  fTabVGroup.InsertFront(fTabHGroup);

  fTabCurrent := TCastleRectangleControl.Create(fTabVGroup);
  fTabCurrent.Color := fTabActiveColor;
  fTabCurrent.Height := fTabCaptionHeight;
  fTabCurrent.Width := Width;
  fTabVGroup.InsertFront(fTabCurrent);

  fTabCaption := TCastleLabel.Create(fTabCurrent);
  fTabCaption.Caption := '';
  fTabCaption.Color := Black;
  fTabCaption.PaddingHorizontal := 4;
  fTabCurrent.InsertFront(fTabCaption);

  fTabActive := nil;
  fTabActiveColor := Vector4(1.0, 1.0, 1.0, 1.0);
  fTabHoverColor := Vector4(0.9, 0.9, 0.9, 1.0);
  fTabInActiveColor := Vector4(0.8, 0.8, 0.8, 1.0);
end;

procedure TCastlePageControl.AddTab(const ACaption: String; const AURL: String);
var
  TabIndex: Integer;
begin
  TabIndex := Length(fTabs);
  SetLength(fTabs, TabIndex + 1);

  fTabs[TabIndex] := TCastleTabSheet.Create(Self, ACaption, AURL);

  if fTabActive = nil then
    TabActive := fTabs[TabIndex];
end;

procedure TCastlePageControl.SetTabActiveColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(fTabActiveColor, Value) then
  begin
    fTabActiveColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastlePageControl.SetTabHoverColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(fTabHoverColor, Value) then
  begin
    fTabHoverColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastlePageControl.SetTabInActiveColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(fTabInActiveColor, Value) then
  begin
    fTabInActiveColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastlePageControl.SetTabActive(const AValue: TCastleTabSheet);
begin
  if not(fTabActive = AValue) then
    fTabActive := AValue;
end;

procedure TCastlePageControl.Resize;
begin
  inherited;

  Height := TCastleUserInterface(Parent).Height;
  Width := TCastleUserInterface(Parent).Width;

  fTabSection.Height := TabSectionHeight + TabCaptionHeight;
  fTabSection.Width := Width;
end;

end.

