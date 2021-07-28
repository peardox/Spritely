unit CastlePageControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleLog,
  CastleVectors, CastleControls, CastleColors,
  CastleKeysMouse, CastleUIControls;

type
  { TCastleTabSheet }
  TCastlePageControl = class; // Forward declaration

  TCastleTabSheet = class(TCastleRectangleControl)
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; ACaption: String; AURL: String);
  private
    fPageControl: TCastlePageControl;
    fImage: TCastleImageControl;
    fContent: TCastleUserInterface;
    fCaption: String;
    procedure SetContent(const AContent: TCastleUserInterface);
    procedure DoMouseEnterTab(const Sender: TCastleUserInterface);
    procedure DoMouseLeaveTab(const Sender: TCastleUserInterface);
    procedure DoMousePressTab(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
  public
    procedure Resize; override;
    property Image: TCastleImageControl read fImage write fImage;
    property Content: TCastleUserInterface read fContent write SetContent;
    property Caption: String read fCaption write fCaption;
    property PageControl: TCastlePageControl read fPageControl write fPageControl;
  end;
  TCastleTabSheetArray = Array of TCastleTabSheet;

  { TCastlePageControl }
  TCastlePageControl = class(TCastleUserInterface)
    constructor Create(AOwner: TComponent); override;
  private
    fTabSection: TCastleUserInterface;
    fTabHGroup: TCastleHorizontalGroup;
    fTabVGroup: TCastleVerticalGroup;
    fContainer: TCastleUserInterface;
    fTabs: TCastleTabSheetArray;
    fTabCurrent: TCastleRectangleControl;
    fTabActive: TCastleTabSheet;
    fImgMargin: Single;
    fTabCaption: TCastleLabel;
    fPaddingTop: Single;
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
    procedure AddTab(const ACaption: String; const AURL: String; const AControlSet: TCastleUserInterface = nil);
    procedure ExtResize;
    property Container: TCastleUserInterface read fContainer write fContainer;
    property PaddingTop: Single read fPaddingTop write fPaddingTop;
    property Tabs: TCastleTabSheetArray read fTabs write fTabs;
    property TabHGroup: TCastleHorizontalGroup read fTabHGroup write fTabHGroup;
    property TabActive: TCastleTabSheet read fTabActive write SetTabActive;
    property TabActiveColor: TCastleColor read fTabActiveColor write SetTabActiveColor;
    property TabHoverColor: TCastleColor read fTabHoverColor write SetTabHoverColor;
    property TabInActiveColor: TCastleColor read fTabInActiveColor write SetTabInActiveColor;
  published
    property ImgMargin: Single read fImgMargin write fImgMargin;
    property TabSection: TCastleUserInterface read fTabSection write fTabSection;
    property TabSectionHeight: Single read fTabSectionHeight write fTabSectionHeight;
    property TabCaptionHeight: Single read fTabCaptionHeight write fTabCaptionHeight;
  end;

implementation

uses ControlPanel;

{ TCastleTabSheet }
constructor TCastleTabSheet.Create(AOwner: TComponent);
begin
  inherited;
end;

constructor TCastleTabSheet.Create(AOwner: TComponent; ACaption: String; AURL: String);
begin
  Create(AOwner);

  fPageControl := TCastlePageControl(AOwner);
  fCaption := ACaption;

  Color := TCastlePageControl(Owner).TabInActiveColor;
  Height := TCastlePageControl(Owner).TabSectionHeight;
  Width := TCastlePageControl(Owner).TabSectionHeight;
  OnInternalMouseEnter := @DoMouseEnterTab;
  OnInternalMouseLeave := @DoMouseLeaveTab;
  OnPress := @DoMousePressTab;

  TCastlePageControl(AOwner).TabHGroup.InsertFront(Self);

  fImage := TCastleImageControl.Create(Self);
  fImage.Height := TCastlePageControl(AOwner).TabSectionHeight - TCastlePageControl(AOwner).ImgMargin;
  fImage.Width := TCastlePageControl(AOwner).TabSectionHeight - TCastlePageControl(AOwner).ImgMargin;
  fImage.Left := (TCastlePageControl(AOwner).ImgMargin / 2) - Self.Border.AllSides;
  fImage.Bottom := (TCastlePageControl(AOwner).ImgMargin / 2) - Self.Border.AllSides;
  fImage.URL := AURL;
  fImage.Stretch := True;
  Self.InsertFront(fImage);

end;

procedure TCastleTabSheet.SetContent(const AContent: TCastleUserInterface);
begin
  if not(fContent = AContent) then
    fContent := AContent;
end;

procedure TCastleTabSheet.Resize;
begin
  inherited;
  WriteLnLog('TCastleTabSheet.Resize');
end;

procedure TCastleTabSheet.DoMousePressTab(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
begin
  if not(TCastleTabSheet(Sender).PageControl.TabActive = Sender) then
    TCastleTabSheet(Sender).PageControl.TabActive := TCastleTabSheet(Sender);
end;

procedure TCastleTabSheet.DoMouseEnterTab(const Sender: TCastleUserInterface);
begin
  if not(TCastleTabSheet(Sender).PageControl.TabActive = Sender) then
    begin
      TCastleRectangleControl(Sender).Color := TCastleTabSheet(Sender).PageControl.TabHoverColor;
      TCastleTabSheet(Sender).Image.Left := (TCastleTabSheet(Sender).PageControl.ImgMargin / 2) - Self.Border.AllSides + 1;
      TCastleTabSheet(Sender).Image.Bottom := (TCastleTabSheet(Sender).PageControl.ImgMargin / 2) - Self.Border.AllSides + 1;
    end;
end;

procedure TCastleTabSheet.DoMouseLeaveTab(const Sender: TCastleUserInterface);
begin
  if not(TCastleTabSheet(Sender).PageControl.TabActive = Sender) then
    TCastleRectangleControl(Sender).Color := TCastleTabSheet(Sender).PageControl.TabInActiveColor
  else
    TCastleRectangleControl(Sender).Color := TCastleTabSheet(Sender).PageControl.TabActiveColor;

  TCastleTabSheet(Sender).Image.Left := (TCastleTabSheet(Sender).PageControl.ImgMargin / 2) - Self.Border.AllSides;
  TCastleTabSheet(Sender).Image.Bottom := (TCastleTabSheet(Sender).PageControl.ImgMargin / 2) - Self.Border.AllSides;
end;

{ TCastlePageControl }
constructor TCastlePageControl.Create(AOwner: TComponent);
begin
  inherited;

  fTabSectionHeight := 40;
  fTabCaptionHeight := 24;
  fImgMargin := 8;

  fTabActive := nil;
  fTabActiveColor := Vector4(1.0, 1.0, 1.0, 1.0);
  fTabHoverColor := Vector4(0.9, 0.9, 0.9, 1.0);
  fTabInActiveColor := Vector4(0.8, 0.8, 0.8, 1.0);

  Width := TControlPanel(Owner).Width;
  Height := TControlPanel(Owner).Height;

  fTabSection := TCastleUserInterface.Create(Self);
  fTabSection.Height := fTabSectionHeight + fTabCaptionHeight;
  fTabSection.Width := Width;

  fTabSection.HorizontalAnchorParent := hpLeft;
  fTabSection.HorizontalAnchorSelf := hpLeft;
  fTabSection.HorizontalAnchorDelta := 0;
  fTabSection.VerticalAnchorParent := vpTop;
  fTabSection.VerticalAnchorSelf := vpTop;
  fTabSection.VerticalAnchorDelta := 0;

  Self.InsertFront(fTabSection);

  fTabVGroup := TCastleVerticalGroup.Create(fTabSection);
  fTabVGroup.Height := fTabSectionHeight + fTabCaptionHeight;
  fTabVGroup.Width := Width;

//  fTabVGroup.Border.AllSides := 1;
//  fTabVGroup.BorderColor := Maroon;

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

  fContainer := TCastleUserInterface.Create(Self);

  fContainer.Width := Width;
  fContainer.Height := Height;

  fContainer.HorizontalAnchorParent := hpLeft;
  fContainer.HorizontalAnchorSelf := hpLeft;
  fContainer.HorizontalAnchorDelta := 0;
  fContainer.VerticalAnchorParent := vpTop;
  fContainer.VerticalAnchorSelf := vpTop;
  fContainer.VerticalAnchorDelta := -(TabSectionHeight + TabCaptionHeight);

  Self.InsertFront(fContainer);

  WriteLnLog('TTWidth : ' + FloatToStr(Width) + ' / ' + 'Height : ' + FloatToStr(Height));
  WriteLnLog('TabWidth : ' + FloatToStr(fTabSection.Width) + ' / ' + 'TabHeight : ' + FloatToStr(fTabSection.Height));
  WriteLnLog('TabBottom : ' + FloatToStr(fTabSection.Bottom));
end;

procedure TCastlePageControl.AddTab(const ACaption: String; const AURL: String; const AControlSet: TCastleUserInterface = nil);
var
  TabIndex: Integer;
  ThisTab: TCastleTabSheet;
  ThisContent: TCastleUserInterface;
  ALabel: TCastleLabel;
begin
  TabIndex := Length(fTabs);
  SetLength(fTabs, TabIndex + 1);

  ThisTab := TCastleTabSheet.Create(Self, ACaption, AURL);

  if AControlSet = nil then
    begin
      ThisContent := TCastleUserInterface.Create(ThisTab);
      ThisContent.Width := Width;
      ThisContent.Height := Height;
      ThisContent.HorizontalAnchorParent := hpMiddle;
      ThisContent.HorizontalAnchorSelf := hpMiddle;
      ThisContent.HorizontalAnchorDelta := 0;
      ThisContent.VerticalAnchorParent := vpMiddle;
      ThisContent.VerticalAnchorSelf := vpMiddle;
      ThisContent.VerticalAnchorDelta := 0;

      ALabel := TCastleLabel.Create(ThisContent);
      ALabel.Color := White;
      ALabel.Caption := ACaption;
      ALabel.HorizontalAnchorParent := hpMiddle;
      ALabel.HorizontalAnchorSelf := hpMiddle;
      ALabel.HorizontalAnchorDelta := 0;
      ALabel.VerticalAnchorParent := vpMiddle;
      ALabel.VerticalAnchorSelf := vpMiddle;
      ALabel.VerticalAnchorDelta := 0;
      ThisContent.InsertFront(ALabel);
      ThisTab.Content := ThisContent;
    end
  else
    ThisTab.Content := AControlSet;

  fTabs[TabIndex] := ThisTab;

  if fTabActive = nil then
    TabActive := ThisTab;
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
    begin
      if not(fTabActive = nil) then
        begin
          fTabActive.Color := fTabInActiveColor;
          fContainer.ClearControls;
        end;
      fTabActive := AValue;
      fTabCaption.Caption := fTabActive.fCaption;
      fTabActive.Color := fTabActiveColor;

      fContainer.InsertFront(TabActive.Content);
    end;
end;

procedure TCastlePageControl.ExtResize;
begin
  fTabSection.Height := fTabSectionHeight + fTabCaptionHeight;
  fTabSection.Width := Width;

  fContainer.Height := Height - fTabSection.Height;
  fContainer.Width := Width;

  WriteLnLog('TCastlePageControl.ExtResize');
  WriteLnLog('TTWidth : ' + FloatToStr(Width) + ' / ' + 'Height : ' + FloatToStr(Height));
  WriteLnLog('TabWidth : ' + FloatToStr(fTabSection.Width) + ' / ' + 'TabHeight : ' + FloatToStr(fTabSection.Height));
end;

end.

