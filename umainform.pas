unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Menus, StdCtrls, DividerBevel, VirtualTrees, Clipbrd,
  //DK packages utils
  {DK_HeapTrace,} DK_LCLStrRus, DK_CtrlUtils, DK_VSTTables, DK_VSTTypes,
  DK_Vector, DK_Const, DK_Dialogs, DK_StrUtils,
  //Project utils
  UDataBase, UUtils, UImages,
  //Forms
  UDocumentEditForm, UAddonEditForm, UAboutForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TSpeedButton;
    AddButton: TSpeedButton;
    AddonAddButton: TSpeedButton;
    AddonCaptionPanel: TPanel;
    AddonCaptionSymbolLabel: TLabel;
    AddonDelButton: TSpeedButton;
    AddonEditButton: TSpeedButton;
    AddonInfoPanel: TPanel;
    AddonPDFCopyButton: TSpeedButton;
    AddonPDFShowButton: TSpeedButton;
    AddonToolPanel: TPanel;
    AddonVT: TVirtualStringTree;
    DelButton: TSpeedButton;
    DictionaryButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel10: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    DividerBevel6: TDividerBevel;
    DividerBevel8: TDividerBevel;
    DividerBevel9: TDividerBevel;
    DocCaptionPanel: TPanel;
    DocCaptionSymbolLabel: TLabel;
    DocCodeCaptionLabel: TLabel;
    DocCodeValueLabel: TLabel;
    DocDateCaptionLabel: TLabel;
    DocDateValueLabel: TLabel;
    DocInfoPanel: TPanel;
    DocNameCaptionLabel: TLabel;
    DocNameEdit: TEdit;
    DocNameLabel: TLabel;
    DocNameValueLabel: TLabel;
    DocNoteCaptionLabel: TLabel;
    DocNoteValueLabel: TLabel;
    DocNumEdit: TEdit;
    DocNumLabel: TLabel;
    DocStatusCaptionLabel: TLabel;
    DocStatusComboBox: TComboBox;
    DocStatusValueLabel: TLabel;
    DocTypeComboBox: TComboBox;
    DocTypeLabel: TLabel;
    EditButton: TSpeedButton;
    ExitButton: TSpeedButton;
    ExportButton: TSpeedButton;
    FilterClearButton: TSpeedButton;
    FilterLabel: TLabel;
    FilterPanel: TPanel;
    FilterTimer: TTimer;
    FullNameMenuItem: TMenuItem;
    InfoPanel: TPanel;
    SymbolsMenuItem: TMenuItem;
    NameCopyButton: TSpeedButton;
    MainPanel: TPanel;
    PDFCopyButton: TSpeedButton;
    PDFShowButton: TSpeedButton;
    RefreshButton: TSpeedButton;
    Splitter1: TSplitter;
    StatusLabel: TLabel;
    ToolPanel: TPanel;
    TypeNumMenuItem: TMenuItem;
    NameCopyMenu: TPopupMenu;
    NameMenuItem: TMenuItem;
    DictionaryMenu: TPopupMenu;
    StatusesMenuItem: TMenuItem;
    TypesMenuItem: TMenuItem;
    TypeNumYearMenuItem: TMenuItem;
    VT: TVirtualStringTree;
    procedure AboutButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure AddonAddButtonClick(Sender: TObject);
    procedure AddonCaptionPanelClick(Sender: TObject);
    procedure AddonDelButtonClick(Sender: TObject);
    procedure AddonEditButtonClick(Sender: TObject);
    procedure AddonPDFCopyButtonClick(Sender: TObject);
    procedure AddonPDFShowButtonClick(Sender: TObject);
    procedure AddonVTDblClick(Sender: TObject);
    procedure DocCaptionPanelClick(Sender: TObject);
    procedure FilterTimerTimer(Sender: TObject);
    procedure SymbolsMenuItemClick(Sender: TObject);
    procedure TypeNumMenuItemClick(Sender: TObject);
    procedure TypeNumYearMenuItemClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure FullNameMenuItemClick(Sender: TObject);
    procedure NameCopyButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure DocNameEditChange(Sender: TObject);
    procedure DocNumEditChange(Sender: TObject);
    procedure DocStatusComboBoxChange(Sender: TObject);
    procedure DocTypeComboBoxChange(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FilterClearButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameMenuItemClick(Sender: TObject);
    procedure PDFCopyButtonClick(Sender: TObject);
    procedure PDFShowButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure StatusesMenuItemClick(Sender: TObject);
    procedure TypesMenuItemClick(Sender: TObject);
    procedure VTDblClick(Sender: TObject);
  private
    CanLoadDocList: Boolean;
    CanApplyFilter: Boolean;
    DocList: TVSTTable;
    AddonList: TVSTTable;

    DocIDs, TypeIDs, StatusIDs, SymbolIDs: TIntVector;
    DocDates, ControlDates: TDateVector;
    TypeNames, DocNums, Delimiters, DocYears, DocNames, StatusNames, Notes: TStrVector;

    FilterTypeIDs, FilterStatusIDs: TIntVector;
    DocNameMatchStr: String;

    AddonIDs: TIntVector;
    AddonDates: TDateVector;
    AddonNames, AddonNums, AddonNotes: TStrVector;

    procedure DocListCreate;
    procedure DocListLoad(const ASelectedID: Integer = 0);
    procedure DocListSelect;
    procedure DocListFilter;

    procedure DocInfoUpdate;

    procedure AddonListCreate;
    procedure AddonListLoad(const ASelectedID: Integer = 0);
    procedure AddonListSelect;

    procedure DocumentEditFormOpen(const AEditType: Byte); //0-добавить, 1-редактировать
    procedure DocumentShow;

    procedure AddonEditFormOpen(const AEditType: Byte); //0-добавить, 1-редактировать
    procedure AddonShow;

    procedure FilterClearButtonEnable;

    procedure DBConnect;
    procedure DictionarySelect(const ADictionary: Byte);
    procedure ViewUpdate;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //HeapTraceOutputFile('trace.trc');
  Caption:= 'DKTechDoc v.0.0.1 - Библиотека технических документов';
  DBConnect;

  DataBase.DocTypesLoad(DocTypeComboBox, FilterTypeIDs, True);
  DataBase.DocStatusesLoad(DocStatusComboBox, FilterStatusIDs, True);

  CanLoadDocList:= True;
  CanApplyFilter:= True;
  DocListCreate;
  AddonListCreate;
  DocInfoUpdate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DataBase);
  FreeAndNil(DocList);
  FreeAndNil(AddonList);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, AddonToolPanel
  ]);
  SetCaptionPanels([
    DocCaptionPanel, AddonCaptionPanel
  ]);
  SetToolButtons([
    RefreshButton, AboutButton, ExitButton,
    AddButton, DelButton, EditButton, PDFShowButton, PDFCopyButton,
    ExportButton, FilterClearButton,
    AddonAddButton, AddonDelButton, AddonEditButton, AddonPDFShowButton, AddonPDFCopyButton
  ]);

  Images.ToButtons([
    DictionaryButton, NameCopyButton,
    RefreshButton, AboutButton, ExitButton,
    AddButton, DelButton, EditButton, PDFShowButton, PDFCopyButton,
    ExportButton, FilterClearButton,
    AddonAddButton, AddonDelButton, AddonEditButton, AddonPDFShowButton, AddonPDFCopyButton
  ]);

  DocListLoad;

  MainPanel.Visible:= True;
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ExportButtonClick(Sender: TObject);
begin
  Screen.Cursor:= crHourGlass;
  try
    DocList.Save([ctInteger, //№ п/п
                  ctString,  //тип
                  ctString,  //номер
                  ctDate,    //дата введения в действие
                  ctString,  //статус
                  ctString,  //наименование
                  ctString   //примечание
    ]);
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TMainForm.FilterClearButtonClick(Sender: TObject);
begin
  CanLoadDocList:= False;
  DocStatusComboBox.ItemIndex:= 0;
  DocTypeComboBox.ItemIndex:= 0;
  DocNumEdit.Text:= EmptyStr;
  DocNameEdit.Text:= EmptyStr;
  CanLoadDocList:= True;
  DocListLoad;
end;

procedure TMainForm.DictionaryButtonClick(Sender: TObject);
begin
  ControlPopupMenuShow(Sender, DictionaryMenu);
end;

procedure TMainForm.NameCopyButtonClick(Sender: TObject);
begin
  ControlPopupMenuShow(Sender, NameCopyMenu);
end;

procedure TMainForm.DocListFilter;
begin
  DocListLoad;
  FilterClearButtonEnable;
end;

procedure TMainForm.DocInfoUpdate;
var
  i: Integer;
begin
  if DocList.IsSelected then
  begin
    i:= DocList.SelectedIndex;
    DocCodeValueLabel.Caption:= DocumentCode(TypeNames[i], DocNums[i], Delimiters[i], DocYears[i]);
    DocNameValueLabel.Caption:= DocNames[i];
    DocStatusValueLabel.Caption:= StatusNames[i];
    DocDateValueLabel.Caption:= FormatDateTime('dd.mm.yyyy', DocDates[i]);
    DocNoteValueLabel.Caption:= Notes[i];
  end
  else begin
    DocCodeValueLabel.Caption:= SYMBOL_SPACE;
    DocNameValueLabel.Caption:= SYMBOL_SPACE;
    DocStatusValueLabel.Caption:= SYMBOL_SPACE;
    DocDateValueLabel.Caption:= SYMBOL_SPACE;
    DocNoteValueLabel.Caption:= SYMBOL_SPACE;
  end;
end;

procedure TMainForm.AddonListCreate;
begin
  AddonList:= TVSTTable.Create(AddonVT);
  AddonList.OnSelect:= @AddonListSelect;
  AddonList.CanSelect:= True;
  AddonList.HeaderFont.Style:= [fsBold];
  AddonList.AddColumn('№ п/п', 60);
  AddonList.AddColumn('Дата введения', 130);
  AddonList.AddColumn('Наименование приложения', 500);
  AddonList.AddColumn('Примечание', 250);
  AddonList.AutosizeColumnEnableLast;
end;

procedure TMainForm.AddonListLoad(const ASelectedID: Integer);
var
  i, SelectedAddonID: Integer;
  V: TStrVector;
  S: String;
begin
  if not CanLoadDocList then Exit;

  Screen.Cursor:= crHourGlass;
  try
    SelectedAddonID:= ASelectedID;
    if SelectedAddonID<=0 then
      if AddonList.IsSelected then
        SelectedAddonID:= AddonIDs[AddonList.SelectedIndex];

    AddonList.ValuesClear;
    if DocList.IsSelected then
    begin
      DataBase.AddonListLoad(DocIDs[DocList.SelectedIndex],
                             AddonIDs, AddonDates, AddonNames, AddonNums, AddonNotes);

      V:= VIntToStr(VOrder(Length(AddonIDs)));
      AddonList.SetColumn('№ п/п', V);
      V:= VFormatDateTime('dd.mm.yyyy', AddonDates{, True});
      AddonList.SetColumn('Дата введения', V);
      S:= DocumentCode(TypeNames[DocList.SelectedIndex],
                       DocNums[DocList.SelectedIndex],
                       Delimiters[DocList.SelectedIndex],
                       DocYears[DocList.SelectedIndex]);
      V:= VAddonFullName(S, AddonNames, AddonNums);
      AddonList.SetColumn('Наименование приложения', V, taLeftJustify);
      AddonList.SetColumn('Примечание', AddonNotes, taLeftJustify);
    end;
    AddonList.Draw;

  finally
    Screen.Cursor:= crDefault;
  end;

  if SelectedAddonID=0 then Exit;
  i:= VIndexOf(AddonIDs, SelectedAddonID);
  if i>=0 then
    AddonList.Select(i);
end;

procedure TMainForm.AddonListSelect;
var
  S: String;
begin
  AddonPDFShowButton.Enabled:= False;
  AddonPDFCopyButton.Enabled:= False;
  AddonDelButton.Enabled:= AddonList.IsSelected;
  AddonEditButton.Enabled:= AddonDelButton.Enabled;

  if not DocList.IsSelected then Exit;

  S:= AddonFileName(DocIDs[DocList.SelectedIndex], AddonIDs[AddonList.SelectedIndex]);
  AddonPDFShowButton.Enabled:= AddonDelButton.Enabled and
                               FileExists(S);
  AddonPDFCopyButton.Enabled:= AddonPDFShowButton.Enabled;
end;

procedure TMainForm.DocNameEditChange(Sender: TObject);
var
  NewMatchStr: String;
begin
  NewMatchStr:= PrepareMatchStr(DocNameEdit.Text);
  if SSame(NewMatchStr, DocNameMatchStr) then Exit;
  DocNameMatchStr:= NewMatchStr;

  CanApplyFilter:= False;
  if FilterTimer.Enabled then
    FilterTimer.Enabled:= False;
  FilterTimer.Enabled:= True;
  CanApplyFilter:= True;
end;

procedure TMainForm.FilterTimerTimer(Sender: TObject);
begin
  FilterTimer.Enabled:= False;
  if not CanApplyFilter then Exit;
  DocListFilter;
end;

procedure TMainForm.DocNumEditChange(Sender: TObject);
begin
  DocListFilter;
end;

procedure TMainForm.DocStatusComboBoxChange(Sender: TObject);
begin
  DocListFilter;
end;

procedure TMainForm.DocTypeComboBoxChange(Sender: TObject);
begin
  DocListFilter;
end;

procedure TMainForm.EditButtonClick(Sender: TObject);
begin
  DocumentEditFormOpen(1);
end;

procedure TMainForm.AddButtonClick(Sender: TObject);
begin
  DocumentEditFormOpen(0);
end;

procedure TMainForm.AddonAddButtonClick(Sender: TObject);
begin
  AddonEditFormOpen(0);
end;

procedure TMainForm.AddonDelButtonClick(Sender: TObject);
var
  S: String;
begin
  if not DocList.IsSelected then Exit;
  if not AddonList.IsSelected then Exit;
  S:= DocumentCode(TypeNames[DocList.SelectedIndex],
                    DocNums[DocList.SelectedIndex],
                    Delimiters[DocList.SelectedIndex],
                    DocYears[DocList.SelectedIndex]);
  S:= AddonFullName(S, AddonNames[AddonList.SelectedIndex],
                    AddonNums[AddonList.SelectedIndex]);
  if not Confirm('Удалить "' + S + '"?') then Exit;
  if not DataBase.AddonDelete(AddonIDs[AddonList.SelectedIndex]) then Exit;
  S:= AddonFileName(DocIDs[DocList.SelectedIndex],
                    AddonIDs[AddonList.SelectedIndex]);
  DocumentDelete(S);
  AddonListLoad;
end;

procedure TMainForm.AddonEditButtonClick(Sender: TObject);
begin
  AddonEditFormOpen(1);
end;

procedure TMainForm.AddonPDFCopyButtonClick(Sender: TObject);
var
  SrcFileName, DestFileName, S: String;
begin
  if not DocList.IsSelected then Exit;
  if not AddonList.IsSelected then Exit;

  SrcFileName:= AddonFileName(DocIDs[DocList.SelectedIndex],
                              AddonIDs[AddonList.SelectedIndex]);

  S:= DocumentCode(TypeNames[DocList.SelectedIndex],
                   DocNums[DocList.SelectedIndex],
                   Delimiters[DocList.SelectedIndex],
                   DocYears[DocList.SelectedIndex]);
  DestFileName:= AddonFileName(S,
                               AddonNames[AddonList.SelectedIndex],
                               AddonNums[AddonList.SelectedIndex]);
  if DocumentCopy(SrcFileName, DestFileName) then
    ShowInfo('Выполнено!');
end;

procedure TMainForm.AddonPDFShowButtonClick(Sender: TObject);
begin
  AddonShow;
end;

procedure TMainForm.AddonVTDblClick(Sender: TObject);
begin
  AddonShow;
end;

procedure TMainForm.AddonCaptionPanelClick(Sender: TObject);
begin
  VT.Align:= alTop;
  InfoPanel.Visible:= False;
  Splitter1.Align:= alTop;
  try
    AddonInfoPanel.Visible:= not AddonInfoPanel.Visible;
    if AddonInfoPanel.Visible then
    begin
      InfoPanel.Height:= InfoPanel.Height + AddonInfoPanel.Height;
      AddonCaptionSymbolLabel.Caption:= SYMBOL_COLLAPSE;
    end
    else begin
      InfoPanel.Height:= InfoPanel.Height - AddonInfoPanel.Height;
      AddonCaptionSymbolLabel.Caption:= SYMBOL_DROPDOWN;
    end;

  finally
    InfoPanel.Visible:= True;
    Splitter1.Align:= alBottom;
    VT.Align:= alClient;
  end;
end;

procedure TMainForm.DocCaptionPanelClick(Sender: TObject);
begin
  VT.Align:= alTop;
  InfoPanel.Visible:= False;
  Splitter1.Align:= alTop;
  try
    DocInfoPanel.Visible:= not DocInfoPanel.Visible;
    if DocInfoPanel.Visible then
    begin
      InfoPanel.Height:= InfoPanel.Height + DocInfoPanel.Height;
      DocCaptionSymbolLabel.Caption:= SYMBOL_COLLAPSE;
    end
    else begin
      InfoPanel.Height:= InfoPanel.Height - DocInfoPanel.Height;
      DocCaptionSymbolLabel.Caption:= SYMBOL_DROPDOWN;
    end;

  finally
    InfoPanel.Visible:= True;
    Splitter1.Align:= alBottom;
    VT.Align:= alClient;
  end;
end;

procedure TMainForm.TypeNumMenuItemClick(Sender: TObject);
begin
 Clipboard.AsText:= (Sender as TMenuItem).Caption;
end;

procedure TMainForm.TypeNumYearMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText:= (Sender as TMenuItem).Caption;
end;

procedure TMainForm.NameMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText:= (Sender as TMenuItem).Caption;
end;

procedure TMainForm.FullNameMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText:= (Sender as TMenuItem).Caption;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  FormModalShow(TAboutForm);
end;

procedure TMainForm.DelButtonClick(Sender: TObject);
var
  S: String;
begin
  if not DocList.IsSelected then Exit;
  S:= DocumentCode(TypeNames[DocList.SelectedIndex],
                    DocNums[DocList.SelectedIndex],
                    Delimiters[DocList.SelectedIndex],
                    DocYears[DocList.SelectedIndex]);
  if not Confirm('Удалить "' + S + '"?') then Exit;
  if not DataBase.DocDelete(DocIDs[DocList.SelectedIndex]) then Exit;
  DocumentAndAddonsDelete(DocIDs[DocList.SelectedIndex], AddonIDs);
  DocListLoad;
end;

procedure TMainForm.PDFCopyButtonClick(Sender: TObject);
var
  SrcFileName, DestFileName: String;
begin
  if not DocList.IsSelected then Exit;

  SrcFileName:= DocumentFileName(DocIDs[DocList.SelectedIndex]);
  DestFileName:= DocumentFileName(TypeNames[DocList.SelectedIndex],
                                  DocNums[DocList.SelectedIndex],
                                  Delimiters[DocList.SelectedIndex],
                                  DocYears[DocList.SelectedIndex]);
  if DocumentCopy(SrcFileName, DestFileName) then
    ShowInfo('Выполнено!');
end;

procedure TMainForm.DocumentShow;
begin
  if not DocList.IsSelected then Exit;
  DocumentOpen(DocumentFileName(DocIDs[DocList.SelectedIndex]));
end;

procedure TMainForm.AddonEditFormOpen(const AEditType: Byte);
var
  AddonEditForm: TAddonEditForm;
  AddonID: Integer;
begin
  AddonEditForm:= TAddonEditForm.Create(nil);
  try
    AddonEditForm.DocID:= DocIDs[DocList.SelectedIndex];
    if AEditType=0 then //добавить
      AddonID:= 0
    else begin
      AddonID:= AddonIDs[AddonList.SelectedIndex];
      if SFind(AddonNames[AddonList.SelectedIndex], 'изменение', False) then
        AddonEditForm.ModificationRadioButton.Checked:= True;
      AddonEditForm.NumEdit.Text:= AddonNums[AddonList.SelectedIndex];
      AddonEditForm.DatePicker.Date:= AddonDates[AddonList.SelectedIndex];
      AddonEditForm.NoteEdit.Text:= AddonNotes[AddonList.SelectedIndex];
    end;
    AddonEditForm.EditType:= AEditType;
    AddonEditForm.AddonID:= AddonID;
    if AddonEditForm.ShowModal=mrOK then
    begin
      AddonID:= AddonEditForm.AddonID;
      AddonListLoad(AddonID);
    end;
  finally
    FreeAndNil(AddonEditForm);
  end;
end;

procedure TMainForm.AddonShow;
begin
  if not DocList.IsSelected then Exit;
  if not AddonList.IsSelected then Exit;
  DocumentOpen(AddonFileName(DocIDs[DocList.SelectedIndex],
                             AddonIDs[AddonList.SelectedIndex]));
end;

procedure TMainForm.FilterClearButtonEnable;
begin
  FilterClearButton.Enabled:= (DocStatusComboBox.ItemIndex>0) or
                              (DocTypeComboBox.ItemIndex>0) or
                              (not SEmpty(DocNumEdit.Text)) or
                              (not SEmpty(DocNameEdit.Text));
end;

procedure TMainForm.PDFShowButtonClick(Sender: TObject);
begin
  DocumentShow;
end;

procedure TMainForm.RefreshButtonClick(Sender: TObject);
begin
  DataBase.Reconnect;
  ViewUpdate;
end;

procedure TMainForm.VTDblClick(Sender: TObject);
begin
  DocumentShow;
end;

procedure TMainForm.TypesMenuItemClick(Sender: TObject);
var
  ID, n: Integer;
  S: String;
begin
  DictionarySelect(1);

  S:= DocTypeComboBox.Text;

  CanLoadDocList:= False;
  ID:= 0;
  if DocTypeComboBox.ItemIndex>=0 then
    ID:= FilterTypeIDs[DocTypeComboBox.ItemIndex];
  DataBase.DocTypesLoad(DocTypeComboBox, FilterTypeIDs, True);
  n:= VIndexOf(FilterTypeIDs, ID);
  if n>=0 then
    DocTypeComboBox.ItemIndex:= n;
  CanLoadDocList:= True;

  if not SSame(S, DocTypeComboBox.Text) then
    DocListLoad;
end;

procedure TMainForm.StatusesMenuItemClick(Sender: TObject);
var
  ID, n: Integer;
  S: String;
begin
  DictionarySelect(2);

  S:= DocStatusComboBox.Text;

  CanLoadDocList:= False;
  ID:= 0;
  if DocStatusComboBox.ItemIndex>=0 then
    ID:= FilterStatusIDs[DocStatusComboBox.ItemIndex];
  DataBase.DocStatusesLoad(DocStatusComboBox, FilterStatusIDs, True);
  n:= VIndexOf(FilterStatusIDs, ID);
  if n>=0 then
    DocStatusComboBox.ItemIndex:= n;
  CanLoadDocList:= True;

  if not SSame(S, DocStatusComboBox.Text) then
    DocListLoad;
end;

procedure TMainForm.SymbolsMenuItemClick(Sender: TObject);
begin
  DictionarySelect(3);
  DocListLoad;
end;

procedure TMainForm.DBConnect;
var
  DBPath, DBName, DDLName: String;
begin
  DBPath:= ExtractFilePath(Application.ExeName) + 'db' + DirectorySeparator;
  DBName:= DBPath + 'base.db';
  DDLName:= DBPath + 'ddl.sql';

  DataBase:= TDataBase.Create;
  DataBase.Connect(DBName);

  DataBase.ExecuteScript(DDLName);
  DataBase.ExecuteScript([
    'CREATE VIRTUAL TABLE IF NOT EXISTS DOCS_FTS USING FTS5(DocID, DocName);'
  ]);
end;

procedure TMainForm.DictionarySelect(const ADictionary: Byte);
var
  IsOK: Boolean;
begin
  IsOK:= False;
  case ADictionary of
    1: IsOK:= DataBase.EditList('Типы документов',
                'TYPES', 'TypeID', 'TypeName', True, True{, 400, GridFont});
    2: IsOK:= DataBase.EditList('Статусы документов',
                'STATUSES', 'StatusID', 'StatusName', True, True{, 400, GridFont});
    3: IsOK:= DataBase.EditList('Разделители номера и года',
                'SYMBOLS', 'SymbolID', 'SymbolValue', False, False{, 400, GridFont});
  end;

  if IsOK then ViewUpdate;
end;

procedure TMainForm.ViewUpdate;
begin
  DocListLoad;
end;

procedure TMainForm.DocListCreate;
begin
  DocList:= TVSTTable.Create(VT);
  DocList.OnSelect:= @DocListSelect;
  DocList.CanSelect:= True;
  DocList.HeaderFont.Style:= [fsBold];
  DocList.AddColumn('№ п/п', 60);
  DocList.AddColumn('Дата актуализации', 130);
  DocList.AddColumn('Тип', 130);
  DocList.AddColumn('Номер', 130);
  DocList.AddColumn('Дата введения', 130);
  DocList.AddColumn('Статус', 150);
  DocList.AddColumn('Наименование документа', 250);
  DocList.AddColumn('Примечание', 250);
  DocList.AutosizeColumnEnable('Наименование документа');
end;

procedure TMainForm.DocListLoad(const ASelectedID: Integer);
var
  i, SelectedDocID, FilterTypeID, FilterStatusID: Integer;
  FilterDocNum: String;
  V: TStrVector;
begin
  if not CanLoadDocList then Exit;

  Screen.Cursor:= crHourGlass;
  try
    SelectedDocID:= ASelectedID;
    if SelectedDocID<=0 then
      if DocList.IsSelected then
        SelectedDocID:= DocIDs[DocList.SelectedIndex];

    FilterTypeID:= FilterTypeIDs[DocTypeComboBox.ItemIndex];
    FilterStatusID:= FilterStatusIDs[DocStatusComboBox.ItemIndex];
    FilterDocNum:= STrim(DocNumEdit.Text);

    DataBase.DocListLoad(FilterTypeID, FilterStatusID, FilterDocNum, DocNameMatchStr,
                         DocIDs, TypeIDs, StatusIDs, SymbolIDs, DocDates, ControlDates,
                         TypeNames, DocNums, Delimiters, DocYears, DocNames, StatusNames, Notes);
    ExportButton.Enabled:= not VIsNil(DocIDs);

    for i:= 0 to High(DocNums) do
      DocNums[i]:= SReplace(DocNums[i], SYMBOL_HYPHEN, MDASH_DEFAULT);

    DocList.ValuesClear;
    V:= VIntToStr(VOrder(Length(DocIDs)));
    DocList.SetColumn('№ п/п', V);
    DocList.SetColumn('Тип', TypeNames, taLeftJustify);
    //V:= VDocumentCode(TypeNames, DocNums, DocYears);
    V:= VDocumentNumber(DocNums, Delimiters, DocYears);
    DocList.SetColumn('Номер', V, taLeftJustify);
    V:= VFormatDateTime('dd.mm.yyyy', DocDates{, True});
    DocList.SetColumn('Дата введения', V);
    V:= VFormatDateTime('dd.mm.yyyy', ControlDates, True);
    DocList.SetColumn('Дата актуализации', V);
    DocList.SetColumn('Статус', StatusNames);
    DocList.SetColumn('Наименование документа', DocNames, taLeftJustify);
    DocList.SetColumn('Примечание', Notes, taLeftJustify);
    DocList.Draw;

  finally
    Screen.Cursor:= crDefault;
  end;

  if SelectedDocID=0 then Exit;
  i:= VIndexOf(DocIDs, SelectedDocID);
  if i>=0 then
    DocList.Select(i);
end;

procedure TMainForm.DocListSelect;
var
  i: Integer;
  TypeName: String;
begin
  DelButton.Enabled:= DocList.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;

  PDFShowButton.Enabled:= DelButton.Enabled and
                          FileExists(DocumentFileName(DocIDs[DocList.SelectedIndex]));
  PDFCopyButton.Enabled:= PDFShowButton.Enabled;

  if DocList.IsSelected then
  begin
    i:= DocList.SelectedIndex;
    TypeName:= SReplace(TypeNames[i], SYMBOL_SPACE, SYMBOL_SPACE_NONBREAK);
    TypeNumMenuItem.Caption:= TypeName + SYMBOL_SPACE_NONBREAK + DocNums[i];
    TypeNumYearMenuItem.Caption:= TypeNumMenuItem.Caption + Delimiters[i] + DocYears[i];
    NameMenuItem.Caption:= DocNames[i];
    FullNameMenuItem.Caption:= TypeNumYearMenuItem.Caption + SYMBOL_SPACE_NONBREAK + SRusQuoted(DocNames[i]);
  end;

  DocInfoUpdate;

  NameCopyButton.Enabled:= DocList.IsSelected;
  AddonAddButton.Enabled:= DocList.IsSelected;

  AddonListLoad;
end;

procedure TMainForm.DocumentEditFormOpen(const AEditType: Byte);
var
  DocumentEditForm: TDocumentEditForm;
  DocID: Integer;
begin
  DocumentEditForm:= TDocumentEditForm.Create(nil);
  try
    if AEditType=0 then //добавить
      DocID:= 0
    else begin
      DocID:= DocIDs[DocList.SelectedIndex];
      DocumentEditForm.OldTypeID:= TypeIDs[DocList.SelectedIndex];
      DocumentEditForm.OldStatusID:= StatusIDs[DocList.SelectedIndex];
      DocumentEditForm.OldSymbolID:= SymbolIDs[DocList.SelectedIndex];
      DocumentEditForm.DocNumEdit.Text:= DocNums[DocList.SelectedIndex];
      DocumentEditForm.DocYearEdit.Text:= DocYears[DocList.SelectedIndex];
      DocumentEditForm.DocDatePicker.Date:= DocDates[DocList.SelectedIndex];
      DocumentEditForm.DocNameEdit.Text:= DocNames[DocList.SelectedIndex];
      DocumentEditForm.DocStatusComboBox.Text:= StatusNames[DocList.SelectedIndex];
      DocumentEditForm.NoteEdit.Text:= Notes[DocList.SelectedIndex];
    end;
    DocumentEditForm.EditType:= AEditType;
    DocumentEditForm.DocID:= DocID;
    if DocumentEditForm.ShowModal=mrOK then
    begin
      DocID:= DocumentEditForm.DocID;
      DocListLoad(DocID);
    end;
  finally
    FreeAndNil(DocumentEditForm);
  end;
end;

end.

