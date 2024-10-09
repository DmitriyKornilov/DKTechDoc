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
    DelButton: TSpeedButton;
    AddonDelButton: TSpeedButton;
    DictionaryButton: TSpeedButton;
    DividerBevel10: TDividerBevel;
    DividerBevel8: TDividerBevel;
    AddonEditButton: TSpeedButton;
    DividerBevel9: TDividerBevel;
    FullNameMenuItem: TMenuItem;
    InfoPanel: TPanel;
    CaptionPanel: TPanel;
    InfoLabel: TLabel;
    NameCopyButton: TSpeedButton;
    AddonToolPanel: TPanel;
    AddonPDFCopyButton: TSpeedButton;
    AddonPDFShowButton: TSpeedButton;
    Splitter1: TSplitter;
    TypeNumMenuItem: TMenuItem;
    NameCopyMenu: TPopupMenu;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    DividerBevel6: TDividerBevel;
    DocNameEdit: TEdit;
    DocNumEdit: TEdit;
    DocNameLabel: TLabel;
    DocStatusComboBox: TComboBox;
    DocTypeComboBox: TComboBox;
    DocNumLabel: TLabel;
    NameMenuItem: TMenuItem;
    StatusLabel: TLabel;
    EditButton: TSpeedButton;
    ExitButton: TSpeedButton;
    DictionaryMenu: TPopupMenu;
    ExportButton: TSpeedButton;
    FilterPanel: TPanel;
    FilterLabel: TLabel;
    DocTypeLabel: TLabel;
    PDFCopyButton: TSpeedButton;
    FilterClearButton: TSpeedButton;
    PDFShowButton: TSpeedButton;
    StatusesMenuItem: TMenuItem;
    TypesMenuItem: TMenuItem;
    RefreshButton: TSpeedButton;
    ToolPanel: TPanel;
    TypeNumYearMenuItem: TMenuItem;
    VT: TVirtualStringTree;
    AddonVT: TVirtualStringTree;
    procedure AboutButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure AddonAddButtonClick(Sender: TObject);
    procedure AddonDelButtonClick(Sender: TObject);
    procedure AddonEditButtonClick(Sender: TObject);
    procedure AddonPDFCopyButtonClick(Sender: TObject);
    procedure AddonPDFShowButtonClick(Sender: TObject);
    procedure AddonVTDblClick(Sender: TObject);
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
    DocList: TVSTTable;
    AddonList: TVSTTable;

    DocIDs, TypeIDs, StatusIDs: TIntVector;
    DocDates, ControlDates: TDateVector;
    TypeNames, DocNums, DocYears, DocNames, StatusNames, Notes: TStrVector;

    FilterTypeIDs, FilterStatusIDs: TIntVector;

    AddonIDs: TIntVector;
    AddonDates: TDateVector;
    AddonNames, AddonNums, AddonNotes: TStrVector;

    procedure DocListCreate;
    procedure DocListLoad(const ASelectedID: Integer = 0);
    procedure DocListSelect;
    procedure DocListFilter;

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
  DocListCreate;
  AddonListCreate;
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
    CaptionPanel
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
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ExportButtonClick(Sender: TObject);
begin
  DocList.Save([ctInteger, //№ п/п
                ctString,  //тип
                ctString,  //номер
                ctDate,    //дата введения в действие
                ctString,  //статус
                ctString,  //наименование
                ctString   //примечание
  ]);
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
  AddonDelButton.Enabled:= AddonList.IsSelected;
  AddonEditButton.Enabled:= AddonDelButton.Enabled;

  S:= AddonFileName(DocIDs[DocList.SelectedIndex], AddonIDs[AddonList.SelectedIndex]);
  AddonPDFShowButton.Enabled:= DocList.IsSelected and
                               AddonDelButton.Enabled and
                               FileExists(S);
  AddonPDFCopyButton.Enabled:= AddonPDFShowButton.Enabled;
end;

procedure TMainForm.DocNameEditChange(Sender: TObject);
begin
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

procedure TMainForm.DBConnect;
var
  DBPath, DBName, DDLName: String;
begin
  DBPath:= ExtractFilePath(Application.ExeName) + DirectorySeparator + 'db' + DirectorySeparator;
  DBName:= DBPath + 'base.db';
  DDLName:= DBPath + 'ddl.sql';

  DataBase:= TDataBase.Create;
  DataBase.Connect(DBName);
  DataBase.ExecuteScript(DDLName);
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
  DocList.AddColumn('Тип', 130);
  DocList.AddColumn('Номер', 130);
  DocList.AddColumn('Дата введения', 130);
  DocList.AddColumn('Дата актуализации', 130);
  DocList.AddColumn('Статус', 150);
  DocList.AddColumn('Наименование документа', 250);
  DocList.AddColumn('Примечание', 250);
  DocList.AutosizeColumnEnable('Наименование документа');
end;

procedure TMainForm.DocListLoad(const ASelectedID: Integer);
var
  i, SelectedDocID, FilterTypeID, FilterStatusID: Integer;
  FilterDocNum, FilterDocName: String;
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
    FilterDocName:= STrim(DocNameEdit.Text);

    DataBase.DocListLoad(FilterTypeID, FilterStatusID, FilterDocNum, FilterDocName,
                         DocIDs, TypeIDs, StatusIDs, DocDates, ControlDates,
                         TypeNames, DocNums, DocYears, DocNames, StatusNames, Notes);
    ExportButton.Enabled:= not VIsNil(DocIDs);

    DocList.ValuesClear;
    V:= VIntToStr(VOrder(Length(DocIDs)));
    DocList.SetColumn('№ п/п', V);
    DocList.SetColumn('Тип', TypeNames, taLeftJustify);
    //V:= VDocumentCode(TypeNames, DocNums, DocYears);
    V:= VDocumentNumber(DocNums, DocYears);
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
  FullName: String;
begin
  DelButton.Enabled:= DocList.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;

  PDFShowButton.Enabled:= DelButton.Enabled and
                          FileExists(DocumentFileName(DocIDs[DocList.SelectedIndex]));
  PDFCopyButton.Enabled:= PDFShowButton.Enabled;

  CaptionPanel.Caption:= '  Документ не выбран';
  if DocList.IsSelected then
  begin
    i:= DocList.SelectedIndex;

    FullName:=  DocumentFullName(TypeNames[i], DocNums[i], DocYears[i], DocNames[i]);
    CaptionPanel.Caption:= SRepeat(2, SYMBOL_SPACE) + FullName;

    TypeNumMenuItem.Caption:= TypeNames[i] + SYMBOL_SPACE + DocNums[i];
    TypeNumYearMenuItem.Caption:= DocumentCode(TypeNames[i], DocNums[i], DocYears[i]);
    NameMenuItem.Caption:= DocNames[i];
    FullNameMenuItem.Caption:= FullName;
  end;

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

