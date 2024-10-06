unit UDocumentEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, DateTimePicker,
  //DK packages utils
  DK_CtrlUtils, DK_StrUtils, DK_Vector, DK_Dialogs,
  //Project utils
  UDataBase, UUtils, UImages;

type

  { TDocumentEditForm }

  TDocumentEditForm = class(TForm)
    ControlDatePicker: TDateTimePicker;
    DocNameEdit: TEdit;
    DocumentLabel: TLabel;
    FileLabel: TLabel;
    FileNameEdit: TEdit;
    NotChangeFileCheckBox: TCheckBox;
    NoteLabel: TLabel;
    NoteEdit: TEdit;
    DocStatusComboBox: TComboBox;
    DocNameLabel: TLabel;
    OpenButton: TSpeedButton;
    StatusLabel: TLabel;
    DocDatePicker: TDateTimePicker;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    DocDateLabel: TLabel;
    DocYearEdit: TEdit;
    DocYearLabel: TLabel;
    DocTypeComboBox: TComboBox;
    DocTypeLabel: TLabel;
    DocNumLabel: TLabel;
    DocNumEdit: TEdit;
    SaveButton: TSpeedButton;
    ControlDateLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NotChangeFileCheckBoxChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    TypeIDs, StatusIDs: TIntVector;
  public
    DocID, OldTypeID, OldStatusID: Integer;
    EditType: Byte;
  end;

var
  DocumentEditForm: TDocumentEditForm;

implementation

{$R *.lfm}

{ TDocumentEditForm }

procedure TDocumentEditForm.FormShow(Sender: TObject);
begin
  FormKeepMinSize(Self);
  FormToScreenCenter(Self);

  DataBase.DocTypesLoad(DocTypeComboBox, TypeIDs);
  if (not VIsNil(TypeIDs)) and (OldTypeID>0) then
    DocTypeComboBox.ItemIndex:= VIndexOf(TypeIDs, OldTypeID);

  DataBase.DocStatusesLoad(DocStatusComboBox, StatusIDs);
  if (not VIsNil(StatusIDs)) and (OldStatusID>0) then
    DocStatusComboBox.ItemIndex:= VIndexOf(StatusIDs, OldStatusID);
end;

procedure TDocumentEditForm.NotChangeFileCheckBoxChange(Sender: TObject);
begin
  FileNameEdit.Text:= EmptyStr;
  FileNameEdit.Enabled:= not NotChangeFileCheckBox.Checked;
  OpenButton.Enabled:= FileNameEdit.Enabled;
end;

procedure TDocumentEditForm.OpenButtonClick(Sender: TObject);
begin
  DocumentChoose(FileNameEdit);
end;

procedure TDocumentEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TDocumentEditForm.FormCreate(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton, OpenButton]);

  DocDatePicker.Date:= Date;
  ControlDatePicker.Date:= Date;
  OldTypeID:= 0;
  OldStatusID:= 0;
end;

procedure TDocumentEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  TypeID, StatusID: Integer;
  DocNum, DocYear, DocName, Note, SrcFileName, DestFileName: String;
begin
  IsOK:= False;

  DocNum:= STrim(DocNumEdit.Text);
  if SEmpty(DocNum) then
  begin
    ShowInfo('Не указан номер документа!');
    Exit;
  end;

  DocYear:= STrim(DocYearEdit.Text);
  if SEmpty(DocYear) then
  begin
    ShowInfo('Не указан год документа!');
    Exit;
  end;

  TypeID:= TypeIDs[DocTypeComboBox.ItemIndex];
  if DataBase.DocIsExists(DocID, TypeID, DocNum, DocYear) then
  begin
    DocName:= DocumentCode(DocTypeComboBox.Text, DocNum, DocYear);
    if not Confirm('Документ "' + DocName + '" уже есть в базе! Продолжить запись?') then Exit;
  end;

  DocName:= STrim(DocNameEdit.Text);
  if SEmpty(DocName) then
  begin
    ShowInfo('Не указано наименование документа!');
    Exit;
  end;

  SrcFileName:= EmptyStr;
  if not NotChangeFileCheckBox.Checked then
  begin
    SrcFileName:= STrim(FileNameEdit.Text);
    if SEmpty(SrcFileName) then
    begin
      ShowInfo('Не указан файл документа!');
      Exit;
    end;
  end;

  Note:= STrim(NoteEdit.Text);
  StatusID:= StatusIDs[DocStatusComboBox.ItemIndex];

  case EditType of
  0: IsOK:= DataBase.DocAdd(DocID, TypeID, StatusID,
                            DocDatePicker.Date, ControlDatePicker.Date,
                            DocNum, DocYear, DocName, Note);
  1: IsOK:= DataBase.DocUpdate(DocID, TypeID, StatusID,
                            DocDatePicker.Date, ControlDatePicker.Date,
                            DocNum, DocYear, DocName, Note);
  end;

  if not IsOK then Exit;

  if not NotChangeFileCheckBox.Checked then
  begin
    DestFileName:= DocumentFileName(DocID);
    DocumentCopy(SrcFileName, DestFileName, False {no save dialog});
  end;

  ModalResult:= mrOK;
end;

end.

