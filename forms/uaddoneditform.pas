unit UAddonEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, DateTimePicker,
  //DK packages utils
  DK_CtrlUtils, DK_StrUtils, DK_Dialogs,
  //Project utils
  UDataBase, UUtils, UImages;

type

  { TAddonEditForm }

  TAddonEditForm = class(TForm)
    AddonLabel: TLabel;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    CorrectionRadioButton: TRadioButton;
    DocDateLabel: TLabel;
    DatePicker: TDateTimePicker;
    FileLabel: TLabel;
    FileNameEdit: TEdit;
    NotChangeFileCheckBox: TCheckBox;
    OpenButton: TSpeedButton;
    SaveButton: TSpeedButton;
    TypeLabel: TLabel;
    NumLabel: TLabel;
    NumEdit: TEdit;
    ModificationRadioButton: TRadioButton;
    NoteEdit: TEdit;
    NoteLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NotChangeFileCheckBoxChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    DocID, AddonID: Integer;
    EditType: Byte;
  end;

var
  AddonEditForm: TAddonEditForm;

implementation

{$R *.lfm}

{ TAddonEditForm }

procedure TAddonEditForm.FormShow(Sender: TObject);
begin
  FormKeepMinSize(Self);
  FormToScreenCenter(Self);
end;

procedure TAddonEditForm.NotChangeFileCheckBoxChange(Sender: TObject);
begin
  FileNameEdit.Text:= EmptyStr;
  FileNameEdit.Enabled:= not NotChangeFileCheckBox.Checked;
  OpenButton.Enabled:= FileNameEdit.Enabled;
end;

procedure TAddonEditForm.OpenButtonClick(Sender: TObject);
begin
  DocumentChoose(FileNameEdit);
end;

procedure TAddonEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  SrcFileName, DestFileName, AddonName, AddonNum, Note: String;
begin
  IsOK:= False;

  SrcFileName:= EmptyStr;
  if not NotChangeFileCheckBox.Checked then
  begin
    SrcFileName:= STrim(FileNameEdit.Text);
    if SEmpty(SrcFileName) then
    begin
      ShowInfo('Не указан файл приложения!');
      Exit;
    end;
  end;

  if CorrectionRadioButton.Checked then
    AddonName:= 'Поправка'
  else
    AddonName:= 'Изменение';
  AddonNum:= STrim(NumEdit.Text);
  Note:= STrim(NoteEdit.Text);

  case EditType of
  0: IsOK:= DataBase.AddonAdd(AddonID, DocID, DatePicker.Date, AddonName, AddonNum, Note);
  1: IsOK:= DataBase.AddonUpdate(AddonID, {DocID,} DatePicker.Date, AddonName, AddonNum, Note);
  end;

  if not IsOK then Exit;

  if not NotChangeFileCheckBox.Checked then
  begin
    DestFileName:= AddonFileName(DocID, AddonID);
    DocumentCopy(SrcFileName, DestFileName, False {no save dialog});
  end;

  ModalResult:= mrOK;
end;

procedure TAddonEditForm.FormCreate(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton, OpenButton]);
  DatePicker.Date:= Date;
end;

procedure TAddonEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

