unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Menus, StdCtrls, DividerBevel, VirtualTrees,
  //DK packages utils
  DK_HeapTrace, DK_LCLStrRus, DK_CtrlUtils, DK_VSTTables, DK_VSTTypes,
  DK_Vector, DK_Const, DK_Dialogs, DK_StrUtils,
  //Project utils
  UDataBase, UUtils, UImages,
  //Forms
  UDocumentEditForm, UAboutForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TSpeedButton;
    AddButton: TSpeedButton;
    DelButton: TSpeedButton;
    DictionaryButton: TSpeedButton;
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
    VT: TVirtualStringTree;
    procedure AboutButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
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
    procedure PDFCopyButtonClick(Sender: TObject);
    procedure PDFShowButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure StatusesMenuItemClick(Sender: TObject);
    procedure TypesMenuItemClick(Sender: TObject);
    procedure VTDblClick(Sender: TObject);
  private
    DocList: TVSTTable;

    DocIDs, TypeIDs, StatusIDs: TIntVector;
    DocDates: TDateVector;
    TypeNames, DocNums, DocYears, DocNames, StatusNames, Notes: TStrVector;

    FilterTypeIDs, FilterStatusIDs: TIntVector;

    procedure DocListCreate;
    procedure DocListLoad(const ASelectedID: Integer = 0);
    procedure DocListSelect;
    procedure DocListFilter;

    procedure DocumentEditFormOpen(const AEditType: Byte); //0-добавить, 1-редактировать
    procedure DocumentShow;

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
  HeapTraceOutputFile('trace.trc');
  Caption:= 'DKTechDoc v.0.0.1 - Библиотека технических документов';
  DBConnect;

  DataBase.DocTypesLoad(DocTypeComboBox, FilterTypeIDs, True);
  DataBase.DocStatusesLoad(DocStatusComboBox, FilterStatusIDs, True);

  DocListCreate;
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
  DocStatusComboBox.ItemIndex:= 0;
  DocTypeComboBox.ItemIndex:= 0;
  DocNumEdit.Text:= EmptyStr;
  DocNameEdit.Text:= EmptyStr;
end;

procedure TMainForm.DictionaryButtonClick(Sender: TObject);
begin
  ControlPopupMenuShow(Sender, DictionaryMenu);
end;

procedure TMainForm.DocListFilter;
begin
  DocListLoad;
  FilterClearButtonEnable;
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
  S:= DocumentFileName(DocIDs[DocList.SelectedIndex]);
  DocumentDelete(S);
  DocListLoad;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DataBase);
  FreeAndNil(DocList);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    RefreshButton, AboutButton, ExitButton,
    AddButton, DelButton, EditButton, PDFShowButton, PDFCopyButton,
    ExportButton, FilterClearButton
  ]);

  Images.ToButtons([
    DictionaryButton,
    RefreshButton, AboutButton, ExitButton,
    AddButton, DelButton, EditButton, PDFShowButton, PDFCopyButton,
    ExportButton, FilterClearButton
  ]);

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
begin
  DictionarySelect(1);
end;

procedure TMainForm.StatusesMenuItemClick(Sender: TObject);
begin
  DictionarySelect(2);
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
  DocList.AddColumn('Статус', 150);
  DocList.AddColumn('Наименование', 250);
  DocList.AddColumn('Примечание', 250);
  DocList.AutosizeColumnEnable('Наименование');
end;

procedure TMainForm.DocListLoad(const ASelectedID: Integer);
var
  i, SelectedDocID, FilterTypeID, FilterStatusID: Integer;
  FilterDocNum, FilterDocName: String;
  V: TStrVector;
begin
  SelectedDocID:= ASelectedID;
  if SelectedDocID<=0 then
    if DocList.IsSelected then
      SelectedDocID:= DocIDs[DocList.SelectedIndex];

  FilterTypeID:= FilterTypeIDs[DocTypeComboBox.ItemIndex];
  FilterStatusID:= FilterStatusIDs[DocStatusComboBox.ItemIndex];
  FilterDocNum:= STrim(DocNumEdit.Text);
  FilterDocName:= STrim(DocNameEdit.Text);

  DataBase.DocListLoad(FilterTypeID, FilterStatusID, FilterDocNum, FilterDocName,
                       DocIDs, TypeIDs, StatusIDs, DocDates,
                       TypeNames, DocNums, DocYears, DocNames, StatusNames, Notes);
  ExportButton.Enabled:= not VIsNil(DocIDs);

  V:= VIntToStr(VOrder(Length(DocIDs)));
  DocList.SetColumn('№ п/п', V);
  DocList.SetColumn('Тип', TypeNames, taLeftJustify);
  //V:= VDocumentCode(TypeNames, DocNums, DocYears);
  V:= VDocumentNumber(DocNums, DocYears);
  DocList.SetColumn('Номер', V, taLeftJustify);
  V:= VFormatDateTime('dd.mm.yyyy', DocDates);
  DocList.SetColumn('Дата введения', V);
  DocList.SetColumn('Статус', StatusNames);
  DocList.SetColumn('Наименование', DocNames, taLeftJustify);
  DocList.SetColumn('Примечание', Notes, taLeftJustify);
  DocList.Draw;

  if SelectedDocID=0 then Exit;
  i:= VIndexOf(DocIDs, SelectedDocID);
  if i>=0 then
    DocList.Select(i);
end;

procedure TMainForm.DocListSelect;
begin
  DelButton.Enabled:= DocList.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;

  PDFShowButton.Enabled:= DelButton.Enabled and
                          FileExists(DocumentFileName(DocIDs[DocList.SelectedIndex]));
  PDFCopyButton.Enabled:= PDFShowButton.Enabled;
end;

procedure TMainForm.DocumentEditFormOpen(const AEditType: Byte);
var
  DocumentEditForm: TDocumentEditForm;
  DocID: Integer;
begin
  {DocIDs: TIntVector;
    DocDates: TDateVector;
    TypeNames, DocNums, DocYears, DocNames, StatusNames, Notes: TStrVector;  }

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

