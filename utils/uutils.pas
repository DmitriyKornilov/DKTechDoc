unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StdCtrls, LCLIntf, Forms, FileUtil,

  //DK packages utils
  DK_StrUtils, DK_Const, DK_Vector, DK_Matrix, DK_Dialogs;

function DocumentNumber(const ADocNum, ADocYear: String): String;
function VDocumentNumber(const ADocNums, ADocYears: TStrVector): TStrVector;
function DocumentCode(const ATypeName, ADocNum, ADocYear: String): String;
function VDocumentCode(const ATypeNames, ADocNums, ADocYears: TStrVector): TStrVector;
procedure VDocumentCodeSort(const ACodes: TStrVector; out AIndexes: TIntVector);

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

procedure VDocumentCodeSort(const ACodes: TStrVector; out AIndexes: TIntVector);
var
  i, j, Count, n1, n2: Integer;
  V: TIntVector;
  M: TIntMatrix;

  procedure DoUpdate(const ANumPartIndex, ARangeBeginIndex, ARangeEndIndex: Integer);
  var
    k: Integer;
    Values, OutputIndexes, SortIndexes: TIntVector;
  begin
    //получаем индексы в диапазоне
    OutputIndexes:= VCut(AIndexes, ARangeBeginIndex, ARangeEndIndex);
    //получаем коды в диапазоне
    Values:= VCut(M[ANumPartIndex], ARangeBeginIndex, ARangeEndIndex);
    //сортируем
    VSort(Values, SortIndexes);
    OutputIndexes:= VReplace(OutputIndexes, SortIndexes);
    //записываем в итоговый вектор
    VChangeIn(AIndexes, OutputIndexes, ARangeBeginIndex, ARangeEndIndex);
    //меняем значения кодов
    Values:= VReplace(Values, SortIndexes);
    VChangeIn(M[ANumPartIndex], Values, ARangeBeginIndex, ARangeEndIndex);
    for k:= ANumPartIndex+1 to High(M) do
    begin
      Values:= VCut(M[k], ARangeBeginIndex, ARangeEndIndex);
      Values:= VReplace(Values, SortIndexes);
      VChangeIn(M[k], Values, ARangeBeginIndex, ARangeEndIndex);
    end;
  end;

begin
  AIndexes:= nil;
  if VIsNil(ACodes) then Exit;

  M:= nil;
  for i:= 0 to High(ACodes) do
  begin
    //получаем из кода документа вектор чисел
    V:= VStrToNumbers(ACodes[i]);
    //сохраням этот вектор в матрицу
    MAppend(M, V);
  end;

  //определяем наибольшую длину вектора
  Count:= MMaxLength(M);
  //дополняем вектора до наибольшей длины
  for i:= 0 to High(ACodes) do
    VRedim(M[i], Count, -1);

  //транспонируем матрицу
  M:= MTranspose(M);
  //получаем индексы сортированного вектора первой части кода
  VSort(M[0], AIndexes);
  //сортируем
  for i:= 0 to High(M) do
    M[i]:= VReplace(M[i], AIndexes);

  for i:= 1 to Count-1 do
  begin
    n1:= 0; //первый индекс диапазона
    for j:= 1 to High(ACodes) do
    begin
      if M[i-1, j]>=0 then //предыдущая и часть кода в текущем индексе существует
      begin
        if M[i-1, j]<>M[i-1, j-1] then //в предыдущей части кода пошло следующее число
        begin
          n2:= j-1; //последний индекс диапазона
          if n2>n1 then  //если диапазон более 1 значения, проводим сортировку
            DoUpdate(i, n1, n2);
          n1:= j;
        end;
      end
      else begin //предыдущая или эта часть кода НЕ существует
        n1:= j; //запоминаем текущий индекс как начало диапазона
      end;
    end;

    //оставшиеся коды
    n2:= High(ACodes);
    if n2>n1 then
      DoUpdate(i, n1, n2);
  end;
end;

//procedure VDocumentCodeSort(const ACodes: TStrVector; out AIndexes: TIntVector);
//var
//  i, j, Count, n1, n2: Integer;
//  V, TmpIndexes, SortIndexes: TIntVector;
//  M: TIntMatrix;
//begin
//  AIndexes:= nil;
//  if VIsNil(ACodes) then Exit;
//
//  M:= nil;
//  for i:= 0 to High(ACodes) do
//  begin
//    //получаем из кода документа вектор чисел
//    V:= VStrToNumbers(ACodes[i]);
//    //сохраням этот вектор в матрицу
//    MAppend(M, V);
//  end;
//
//  //определяем наибольшую длину вектора
//  Count:= MMaxLength(M);
//  //дополняем вектора до наибольшей длины
//  for i:= 0 to High(ACodes) do
//    VRedim(M[i], Count, -1);
//
//  //транспонируем матрицу
//  M:= MTranspose(M);
//  //получаем индексы сортированного вектора первой части кода
//  VSort(M[0], AIndexes);
//
//  n1:= 0;
//  for j:= 1 to High(ACodes) do
//  begin
//    if M[0, j]>=0 then //предыдущая и часть кода в текущем индексе существует
//    begin
//      if M[0, j]<>M[0, j-1] then //в предыдущей части кода пошло следующее число
//      begin
//        n2:= j-1;
//        if n2>n1 then
//        begin
//          //получаем индексы в диапазоне
//          TmpIndexes:= VCut(AIndexes, n1, n2);
//          //получаем коды в диапазоне
//          V:= VCut(M[1], n1, n2);
//          //сортируем
//          VSort(V, SortIndexes);
//          TmpIndexes:= VReplace(TmpIndexes, SortIndexes);
//          //записываем в итоговый вектор
//          VChangeIn(AIndexes, TmpIndexes, n1, n2);
//        end;
//        n1:= j;
//      end;
//
//    end
//    else begin //предыдущая или эта часть кода НЕ существует
//      n1:= j; //запоминаем текущий индекс
//    end;
//
//  end;
//
//  //оставшиеся коды
//  n2:= High(ACodes);
//  if n2>n1 then
//  begin
//    //получаем индексы в диапазоне
//    TmpIndexes:= VCut(AIndexes, n1, n2);
//    //получаем коды в диапазоне
//    V:= VCut(M[1], n1, n2);
//    //сортируем
//    VSort(V, SortIndexes);
//    TmpIndexes:= VReplace(TmpIndexes, SortIndexes);
//    //записываем в итоговый вектор
//    VChangeIn(AIndexes, TmpIndexes, n1, n2);
//  end;
//
//
//  //for i:= 1 to Count-1 do
//  //begin
//  //
//  //end;
//
//  //V:= VReplace(M[0], AIndexes);
//
//  //VShowAdd(M[0]);
//  //VShowAdd(V);
//  //VShowAdd(AIndexes);
//  //VShow;
//
//end;

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

