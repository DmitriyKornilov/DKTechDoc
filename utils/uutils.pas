unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StdCtrls, LCLIntf, Forms, FileUtil,

  //DK packages utils
  DK_StrUtils, DK_Const, DK_Vector, DK_Dialogs;

function DocumentNumber(const ADocNum, ADocYear: String): String;
function VDocumentNumber(const ADocNums, ADocYears: TStrVector): TStrVector;
function DocumentCode(const ATypeName, ADocNum, ADocYear: String): String;
function VDocumentCode(const ATypeNames, ADocNums, ADocYears: TStrVector): TStrVector;

function DocumentFullName(const ATypeName, ADocNum, ADocYear, ADocName: String): String;

function DocumentFileName(const ADocID: Integer): String;
function DocumentFileName(const ATypeName, ADocNum, ADocYear: String): String;

function DocumentChoose(const AEdit: TEdit = nil): String;
function DocumentOpen(const AFileName: String): Boolean;
function DocumentDelete(const AFileName: String): Boolean;

function DocumentCopy(const ASrcFileName, ADestFileName: String;
                      const ASaveDialog: Boolean = True): Boolean;


implementation

function DocumentNumber(const ADocNum, ADocYear: String): String;
begin
  Result:= ADocNum + SYMBOL_MIDDASH + ADocYear;
end;

function VDocumentNumber(const ADocNums, ADocYears: TStrVector): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ADocNums) then Exit;

  for i:= 0 to High(ADocNums) do
    VAppend(Result, DocumentNumber(ADocNums[i], ADocYears[i]));
end;

function DocumentCode(const ATypeName, ADocNum, ADocYear: String): String;
begin
  Result:= ATypeName + SYMBOL_SPACE + DocumentNumber(ADocNum, ADocYear);
end;

function VDocumentCode(const ATypeNames, ADocNums, ADocYears: TStrVector): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ATypeNames) then Exit;

  for i:= 0 to High(ATypeNames) do
    VAppend(Result, DocumentCode(ATypeNames[i], ADocNums[i], ADocYears[i]));
end;

function DocumentFullName(const ATypeName, ADocNum, ADocYear, ADocName: String): String;
begin
  Result:= DocumentCode(ATypeName, ADocNum, ADocYear) +
           SYMBOL_SPACE + SRusQuoted(ADocName);
  //Result:= DocumentCode(ATypeName, ADocNum, ADocYear) +
  //         SYMBOL_DOT + SYMBOL_SPACE + ADocName;
end;

function DocumentChoose(const AEdit: TEdit = nil): String;
var
  D: TOpenDialog;
begin
  Result:= EmptyStr;
  D:= TOpenDialog.Create(nil);
  try
    D.Filter:= 'Документ PDF|*.pdf';
    if D.Execute then
    begin
      Result:= D.FileName;
      if Assigned(AEdit) then
      begin
        AEdit.ReadOnly:= False;
        AEdit.Text:= Result;
        AEdit.ReadOnly:= True;
      end;
    end;
  finally
    FreeAndNil(D);
  end;
end;

function DocumentFileName(const ADocID: Integer): String;
begin
  Result:= ExtractFilePath(Application.ExeName) + DirectorySeparator +
           'files' + DirectorySeparator + IntToStr(ADocID) + '.pdf';
end;

function DocumentFileName(const ATypeName, ADocNum, ADocYear: String): String;
begin
  Result:= DocumentCode(ATypeName, ADocNum, ADocYear) + '.pdf';
end;

function DocumentOpen(const AFileName: String): Boolean;
begin
  Result:= False;
  if SEmpty(AFileName) or (not FileExists(AFileName)) then Exit;
  Result:= OpenDocument(AFileName);
end;

function DocumentDelete(const AFileName: String): Boolean;
begin
  Result:= False;
  if not FileExists(AFileName) then Exit;
  Result:= DeleteFile(AFileName);
  if not Result then
    ShowInfo('Не удалось удалить файл' + SYMBOL_BREAK + AFileName);
end;

function DocumentCopy(const ASrcFileName, ADestFileName: String;
                      const ASaveDialog: Boolean = True): Boolean;
var
  D: TSaveDialog;
  FileName: String;
  IsOK: Boolean;
begin
  Result:= False;

  FileName:= ADestFileName;
  if not SFind(ADestFileName, '.pdf', False) then
    FileName:= FileName + '.pdf';

  IsOK:= True;
  if ASaveDialog then
  begin
    D:= TSaveDialog.Create(nil);
    try
      D.Filter:= 'Файл PDF|*.pdf';
      D.FileName:= FileName;
      if D.Execute then
      begin
        FileName:= D.FileName;
        if not SFind(FileName, '.pdf', False) then
          FileName:= FileName + '.pdf';
      end
      else
        IsOK:= False;
    finally
      FreeAndNil(D);
    end;
  end;

  if not IsOK then Exit;
  Result:= CopyFile(ASrcFileName, FileName,
                   [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);
end;


end.

