unit UDataBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  //DK packages utils
  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_StrUtils;

type

  { TDataBase }

  TDataBase = class (TSQLite3)
  public
    function DocListLoad(const AFilterTypeID, AFilterStatusID: Integer;
                         const AFilterDocNum, AFilterDocName: String;
                         out ADocIDs, ATypeIDs, AStatusIDs: TIntVector;
                         out ADocDates: TDateVector;
                         out ATypeNames, ADocNums, ADocYears, ADocNames,
                             AStatusNames, ANotes: TStrVector): Boolean;


    function DocAdd(out ADocID: Integer;
                    const ATypeID, AStatusID: Integer;
                    const ADocDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
    function DocUpdate(const ADocID, ATypeID, AStatusID: Integer;
                    const ADocDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
    function DocDelete(const ADocID: Integer): Boolean;
    function DocIsExists(const ADocID, ATypeID: Integer;
                         const ADocNum, ADocYear: String): Boolean;

    procedure DocTypesLoad(const AComboBox: TComboBox; out AIDs: TIntVector;
                           const ANeedCustomZeroID: Boolean = False);
    procedure DocStatusesLoad(const AComboBox: TComboBox; out AIDs: TIntVector;
                           const ANeedCustomZeroID: Boolean = False);
  end;

var
  DataBase: TDataBase;

implementation

{ TDataBase }

function TDataBase.DocListLoad(const AFilterTypeID, AFilterStatusID: Integer;
                               const AFilterDocNum, AFilterDocName: String;
                               out ADocIDs, ATypeIDs, AStatusIDs: TIntVector;
                               out ADocDates: TDateVector;
                               out ATypeNames, ADocNums, ADocYears, ADocNames,
                                   AStatusNames, ANotes: TStrVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  ADocIDs:= nil;
  ATypeIDs:= nil;
  AStatusIDs:= nil;
  ADocDates:= nil;
  ATypeNames:= nil;
  ADocNums:= nil;
  ADocYears:= nil;
  ADocNames:= nil;
  AStatusNames:= nil;
  ANotes:= nil;

  SQLStr:=
    'SELECT t1.DocID, t1.DocNum, t1.DocYear, t1.DocName, t1.DocDate, t1.Note, ' +
           't1.TypeID, t2.TypeName, ' +
           't1.StatusID, t3.StatusName ' +
    'FROM DOCUMENTS t1 ' +
    'INNER JOIN TYPES t2 ON (t1.TypeID=t2.TypeID) ' +
    'INNER JOIN STATUSES t3 ON (t1.StatusID=t3.StatusID) ' +
    'WHERE (t1.DocID>0) ';

  if AFilterTypeID>0 then
    SQLStr:= SQLStr + 'AND (t1.TypeID = :FilterTypeID) ';
  if AFilterStatusID>0 then
    SQLStr:= SQLStr + 'AND (t1.StatusID = :FilterStatusID) ';
  if not SEmpty(AFilterDocNum) then
    SQLStr:= SQLStr + 'AND (t1.DocNum LIKE :FilterDocNum) ';
  if not SEmpty(AFilterDocName) then
    SQLStr:= SQLStr + 'AND (t1.UpperName LIKE :FilterDocName) ';

  SQLStr:= SQLStr + 'ORDER BY t1.DocNum, t1.DocYear';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamInt('FilterTypeID', AFilterTypeID);
  QParamInt('FilterStatusID', AFilterStatusID);
  QParamStr('FilterDocNum', '%'+AFilterDocNum+'%');
  QParamStr('FilterDocName', '%'+SUpper(AFilterDocName)+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADocIDs, QFieldInt('DocID'));
      VAppend(ATypeIDs, QFieldInt('TypeID'));
      VAppend(AStatusIDs, QFieldInt('StatusID'));

      VAppend(ADocDates, QFieldDT('DocDate'));

      VAppend(ATypeNames, QFieldStr('TypeName'));
      VAppend(ADocNums, QFieldStr('DocNum'));
      VAppend(ADocYears, QFieldStr('DocYear'));
      VAppend(ADocNames, QFieldStr('DocName'));
      VAppend(AStatusNames, QFieldStr('StatusName'));
      VAppend(ANotes, QFieldStr('Note'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.DocAdd(out ADocID: Integer;
                    const ATypeID, AStatusID: Integer;
                    const ADocDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись документа
    QSetSQL(
      sqlINSERT('DOCUMENTS', ['StatusID', 'TypeID',
                              'DocNum', 'DocYear', 'DocName',
                              'DocDate', 'UpperName', 'Note'])
    );
    QParamInt('StatusID', AStatusID);
    QParamInt('TypeID', ATypeID);
    QParamDT('DocDate', ADocDate);

    QParamStr('DocNum', ADocNum);
    QParamStr('DocYear', ADocYear);
    QParamStr('DocName', ADocName);
    QParamStr('UpperName', SUpper(ADocName));
    QParamStr('Note', ANote);
    QExec;
    //получение ID сделанной записи
    ADocID:= LastWritedInt32ID('DOCUMENTS');
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.DocUpdate(const ADocID, ATypeID, AStatusID: Integer;
                    const ADocDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись документа
    QSetSQL(
      sqlUPDATE('DOCUMENTS', ['StatusID', 'TypeID',
                              'DocNum', 'DocYear', 'DocName',
                              'DocDate', 'UpperName', 'Note']) +
      'WHERE DocID = :DocID'
    );
    QParamInt('DocID', ADocID);

    QParamInt('StatusID', AStatusID);
    QParamInt('TypeID', ATypeID);
    QParamDT('DocDate', ADocDate);

    QParamStr('DocNum', ADocNum);
    QParamStr('DocYear', ADocYear);
    QParamStr('DocName', ADocName);
    QParamStr('UpperName', SUpper(ADocName));
    QParamStr('Note', ANote);
    QExec;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.DocDelete(const ADocID: Integer): Boolean;
begin
  Result:= Delete('DOCUMENTS', 'DocID', ADocID);
end;

function TDataBase.DocIsExists(const ADocID, ATypeID: Integer;
                         const ADocNum, ADocYear: String): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DocID ' +
    'FROM DOCUMENTS ' +
    'WHERE (DocID <> :DocID) AND (TypeID = :TypeID) AND ' +
          '(DocNum = :DocNum) AND (DocYear = :DocYear)'
  );
  QParamInt('DocID', ADocID);
  QParamInt('TypeID', ATypeID);
  QParamStr('DocNum', ADocNum);
  QParamStr('DocYear', ADocYear);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

procedure TDataBase.DocTypesLoad(const AComboBox: TComboBox; out AIDs: TIntVector;
                                 const ANeedCustomZeroID: Boolean = False);
var
  S: String;
begin
  S:= EmptyStr;
  if ANeedCustomZeroID then
    S:= 'ВСЕ ТИПЫ';
  KeyPickLoad(AComboBox, AIDs, 'TYPES', 'TypeID', 'TypeName', 'TypeName',
              True{not load zero ID}, S {custom zero ID});
end;

procedure TDataBase.DocStatusesLoad(const AComboBox: TComboBox; out AIDs: TIntVector;
                                    const ANeedCustomZeroID: Boolean = False);
var
  S: String;
begin
  S:= EmptyStr;
  if ANeedCustomZeroID then
    S:= 'ВСЕ СТАТУСЫ';
  KeyPickLoad(AComboBox, AIDs, 'STATUSES', 'StatusID', 'StatusName', 'StatusName',
              True{not load zero ID}, S {custom zero ID});
end;

end.

