unit UDataBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  //DK packages utils
  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_StrUtils, DK_Const,
  //Project utils
  UUtils;

type

  { TDataBase }

  TDataBase = class (TSQLite3)
  public
    function SettingLoad(const ASettingName: String): Integer;
    function SettingsLoad(const ASettingNames: TStrVector): TIntVector;
    procedure SettingUpdate(const ASettingName: String; const ASettingValue: Integer);
    procedure SettingsUpdate(const ASettingNames: TStrVector; const ASettingValues: TIntVector);

    function AddonListLoad(const ADocID: Integer;
                           out AAddonIDs: TIntVector;
                           out AAddonDates: TDateVector;
                           out AAddonNames, AAddonNums, AAddonNotes: TStrVector): Boolean;

    function AddonAdd(out AAddonID: Integer;
                      const ADocID: Integer;
                      const AAddonDate: TDate;
                      const AAddonName, AAddonNum, ANote: String): Boolean;
    function AddonUpdate(const AAddonID{, ADocID}: Integer;
                      const AAddonDate: TDate;
                      const AAddonName, AAddonNum, ANote: String): Boolean;
    function AddonDelete(const AAddonID: Integer): Boolean;

    function DocListLoad(const AFilterTypeID, AFilterStatusID: Integer;
                         const AFilterDocNum, AMatchStr: String;
                         out ADocIDs, ATypeIDs, AStatusIDs, ASymbolIDs: TIntVector;
                         out ADocDates, AControlDates: TDateVector;
                         out ATypeNames, ADocNums, ADelimiters, ADocYears, ADocNames,
                             AStatusNames, ANotes: TStrVector): Boolean;
    function DocFind(const AMatchStr: String; out ADocIDs: TIntVector): Boolean;


    function DocAdd(out ADocID: Integer;
                    const ATypeID, AStatusID, ASymbolID: Integer;
                    const ADocDate, AControlDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
    function DocUpdate(const ADocID, ATypeID, AStatusID, ASymbolID: Integer;
                    const ADocDate, AControlDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
    function DocDelete(const ADocID: Integer): Boolean;
    function DocIsExists(const ADocID, ATypeID, ASymbolID: Integer;
                         const ADocNum, ADocYear: String): Boolean;

    procedure DocTypesLoad(const AComboBox: TComboBox; out AIDs: TIntVector;
                           const ANeedCustomZeroID: Boolean = False);
    procedure DocStatusesLoad(const AComboBox: TComboBox; out AIDs: TIntVector;
                           const ANeedCustomZeroID: Boolean = False);
    procedure SymbolsLoad(const AComboBox: TComboBox; out AIDs: TIntVector);
  end;

var
  DataBase: TDataBase;

implementation

{ TDataBase }

function TDataBase.SettingLoad(const ASettingName: String): Integer;
begin
  Result:= ValueIntByStrID('SETTINGS', 'Value', 'Name', ASettingName);
end;

function TDataBase.SettingsLoad(const ASettingNames: TStrVector): TIntVector;
var
  i: Integer;
begin
  VDim(Result{%H-}, Length(ASettingNames));
  for i:= 0 to High(Result) do
    Result[i]:= SettingLoad(ASettingNames[i]);
end;

procedure TDataBase.SettingUpdate(const ASettingName: String; const ASettingValue: Integer);
begin
  UpdateByStrID('SETTINGS', 'Value', 'Name', ASettingName, ASettingValue, True {commit});
end;

procedure TDataBase.SettingsUpdate(const ASettingNames: TStrVector;
  const ASettingValues: TIntVector);
var
  i: Integer;
begin
  try
    for i:= 0 to High(ASettingNames) do
      UpdateByStrID('SETTINGS', 'Value', 'Name', ASettingNames[i], ASettingValues[i], False {no commit});
    QCommit;
  finally
    QRollback;
  end;
end;

function TDataBase.AddonListLoad(const ADocID: Integer;
                           out AAddonIDs: TIntVector;
                           out AAddonDates: TDateVector;
                           out AAddonNames, AAddonNums, AAddonNotes: TStrVector): Boolean;
begin
  Result:= False;

  AAddonIDs:= nil;
  AAddonDates:= nil;
  AAddonNames:= nil;
  AAddonNums:= nil;
  AAddonNotes:= nil;
  if ADocID<=0 then Exit;

  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('ADDONS', ['AddonID', 'AddonDate', 'AddonName', 'AddonNum', 'AddonNote']) +
    'WHERE DocID = :DocID ' +
    'ORDER BY AddonDate, AddonName, AddonNum'
  );
  QParamInt('DocID', ADocID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AAddonIDs, QFieldInt('AddonID'));
      VAppend(AAddonDates, QFieldDT('AddonDate'));
      VAppend(AAddonNames, QFieldStr('AddonName'));
      VAppend(AAddonNums, QFieldStr('AddonNum'));
      VAppend(AAddonNotes, QFieldStr('AddonNote'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.AddonAdd(out AAddonID: Integer;
                      const ADocID: Integer;
                      const AAddonDate: TDate;
                      const AAddonName, AAddonNum, ANote: String): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись документа
    QSetSQL(
      sqlINSERT('ADDONS', ['DocID', 'AddonDate', 'AddonName', 'AddonNum', 'AddonNote'])
    );
    QParamInt('DocID', ADocID);
    QParamDT('AddonDate', AAddonDate);
    QParamStr('AddonName', AAddonName);
    QParamStr('AddonNum', AAddonNum);
    QParamStr('AddonNote', ANote);
    QExec;
    //получение ID сделанной записи
    AAddonID:= LastWritedInt32ID('ADDONS');
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.AddonUpdate(const AAddonID{, ADocID}: Integer;
                      const AAddonDate: TDate;
                      const AAddonName, AAddonNum, ANote: String): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись документа
    QSetSQL(
      sqlUPDATE('ADDONS', [{'DocID',} 'AddonDate', 'AddonName', 'AddonNum', 'AddonNote']) +
      'WHERE AddonID = :AddonID'
    );
    QParamInt('AddonID', AAddonID);
    //QParamInt('DocID', ADocID);
    QParamDT('AddonDate', AAddonDate);
    QParamStr('AddonName', AAddonName);
    QParamStr('AddonNum', AAddonNum);
    QParamStr('AddonNote', ANote);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.AddonDelete(const AAddonID: Integer): Boolean;
begin
  Result:= Delete('ADDONS', 'AddonID', AAddonID);
end;

function TDataBase.DocListLoad(const AFilterTypeID, AFilterStatusID: Integer;
                         const AFilterDocNum, AMatchStr: String;
                         out ADocIDs, ATypeIDs, AStatusIDs, ASymbolIDs: TIntVector;
                         out ADocDates, AControlDates: TDateVector;
                         out ATypeNames, ADocNums, ADelimiters, ADocYears, ADocNames,
                             AStatusNames, ANotes: TStrVector): Boolean;
var
  SQLStr: String;
  Indexes, MatchIDs: TIntVector;
begin
  Result:= False;

  ADocIDs:= nil;
  ATypeIDs:= nil;
  AStatusIDs:= nil;
  ASymbolIDs:= nil;
  ADocDates:= nil;
  AControlDates:= nil;
  ATypeNames:= nil;
  ADocNums:= nil;
  ADelimiters:= nil;
  ADocYears:= nil;
  ADocNames:= nil;
  AStatusNames:= nil;
  ANotes:= nil;

  MatchIDs:= nil;
  if not SEmpty(AMatchStr) then
    if not DocFind(AMatchStr, MatchIDs) then Exit;

  SQLStr:=
    'SELECT t1.DocID, t1.DocNum, t1.DocYear, t1.DocName, t1.DocDate, t1.ControlDate,  ' +
           't1.Note, t1.TypeID, t1.StatusID, t1.SymbolID, ' +
           't2.TypeName, ' +
           't3.StatusName, ' +
           't4.SymbolValue ' +
    'FROM DOCUMENTS t1 ' +
    'INNER JOIN TYPES t2 ON (t1.TypeID=t2.TypeID) ' +
    'INNER JOIN STATUSES t3 ON (t1.StatusID=t3.StatusID) ' +
    'INNER JOIN SYMBOLS t4 ON (t1.SymbolID=t4.SymbolID) ' +
    'WHERE (t1.DocID>0) ';

  if AFilterTypeID>0 then
    SQLStr:= SQLStr + 'AND (t1.TypeID = :FilterTypeID) ';
  if AFilterStatusID>0 then
    SQLStr:= SQLStr + 'AND (t1.StatusID = :FilterStatusID) ';
  if not SEmpty(AFilterDocNum) then
    SQLStr:= SQLStr + 'AND (t1.DocNum LIKE :FilterDocNum) ';
  if not VIsNil(MatchIDs) then
    SQLStr:= SQLStr + 'AND ' + SQLIN('t1', 'DocID', Length(MatchIDs));
  SQLStr:= SQLStr + 'ORDER BY t1.DocNum, t1.DocYear';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamStr('MatchStr', AMatchStr);
  QParamInt('FilterTypeID', AFilterTypeID);
  QParamInt('FilterStatusID', AFilterStatusID);
  QParamStr('FilterDocNum', AFilterDocNum+'%');
  QParamsInt(MatchIDs);

  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADocIDs, QFieldInt('DocID'));
      VAppend(ATypeIDs, QFieldInt('TypeID'));
      VAppend(AStatusIDs, QFieldInt('StatusID'));
      VAppend(ASymbolIDs, QFieldInt('SymbolID'));

      VAppend(ADocDates, QFieldDT('DocDate'));
      VAppend(AControlDates, QFieldDT('ControlDate'));

      VAppend(ATypeNames, QFieldStr('TypeName'));
      VAppend(ADocNums, QFieldStr('DocNum'));
      VAppend(ADelimiters, QFieldStr('SymbolValue'));
      VAppend(ADocYears, QFieldStr('DocYear'));
      VAppend(ADocNames, QFieldStr('DocName'));
      VAppend(AStatusNames, QFieldStr('StatusName'));
      VAppend(ANotes, QFieldStr('Note'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  VDocumentCodeSort(ADocNums, Indexes);

  ADocIDs:= VReplace(ADocIDs, Indexes);
  ATypeIDs:= VReplace(ATypeIDs, Indexes);
  AStatusIDs:= VReplace(AStatusIDs, Indexes);
  ASymbolIDs:= VReplace(ASymbolIDs, Indexes);
  ADocDates:= VReplace(ADocDates, Indexes);
  AControlDates:= VReplace(AControlDates, Indexes);
  ATypeNames:= VReplace(ATypeNames, Indexes);
  ADocNums:= VReplace(ADocNums, Indexes);
  ADelimiters:= VReplace(ADelimiters, Indexes);
  ADocYears:= VReplace(ADocYears, Indexes);
  ADocNames:= VReplace(ADocNames, Indexes);
  AStatusNames:= VReplace(AStatusNames, Indexes);
  ANotes:= VReplace(ANotes, Indexes);
end;

function TDataBase.DocFind(const AMatchStr: String; out ADocIDs: TIntVector): Boolean;
begin
  ADocIDs:= nil;
  Result:= False;
  if SEmpty(AMatchStr) then Exit;

  ExecuteScript([
    'CREATE VIRTUAL TABLE IF NOT EXISTS DOCS_FTS USING FTS5(DocID, DocName);',
    'INSERT OR IGNORE INTO DOCS_FTS SELECT DocID, DocName FROM DOCUMENTS;'
  ]);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DocID, DocName ' +
    'FROM DOCS_FTS ' +
    'WHERE DOCS_FTS MATCH :MatchStr'
  );
  QParamStr('MatchStr', AMatchStr + '*');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADocIDs, StrToInt(QFieldStr('DocID')));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  ExecuteScript([
    'DELETE FROM DOCS_FTS;'
  ]);
end;

function TDataBase.DocAdd(out ADocID: Integer;
                    const ATypeID, AStatusID, ASymbolID: Integer;
                    const ADocDate, AControlDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись документа
    QSetSQL(
      sqlINSERT('DOCUMENTS', ['StatusID', 'TypeID', 'SymbolID',
                              'DocNum', 'DocYear', 'DocName',
                              'DocDate', 'Note', 'ControlDate'])
    );
    QParamInt('StatusID', AStatusID);
    QParamInt('TypeID', ATypeID);
    QParamInt('SymbolID', ASymbolID);
    QParamDT('DocDate', ADocDate);
    QParamDT('ControlDate', AControlDate);
    QParamStr('DocNum', ADocNum);
    QParamStr('DocYear', ADocYear);
    QParamStr('DocName', ADocName);
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

function TDataBase.DocUpdate(const ADocID, ATypeID, AStatusID, ASymbolID: Integer;
                    const ADocDate, AControlDate: TDate;
                    const ADocNum, ADocYear, ADocName, ANote: String): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись документа
    QSetSQL(
      sqlUPDATE('DOCUMENTS', ['StatusID', 'TypeID', 'SymbolID',
                              'DocNum', 'DocYear', 'DocName',
                              'DocDate', 'Note', 'ControlDate']) +
      'WHERE DocID = :DocID'
    );
    QParamInt('DocID', ADocID);

    QParamInt('StatusID', AStatusID);
    QParamInt('TypeID', ATypeID);
    QParamInt('SymbolID', ASymbolID);
    QParamDT('DocDate', ADocDate);
    QParamDT('ControlDate', AControlDate);
    QParamStr('DocNum', ADocNum);
    QParamStr('DocYear', ADocYear);
    QParamStr('DocName', ADocName);
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

function TDataBase.DocIsExists(const ADocID, ATypeID, ASymbolID: Integer;
                         const ADocNum, ADocYear: String): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DocID ' +
    'FROM DOCUMENTS ' +
    'WHERE (DocID <> :DocID) AND (TypeID = :TypeID) AND ' +
          '(DocNum = :DocNum) AND (DocYear = :DocYear) AND (SymbolID = :SymbolID)'
  );
  QParamInt('DocID', ADocID);
  QParamInt('TypeID', ATypeID);
  QParamInt('SymbolID', ASymbolID);
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

procedure TDataBase.SymbolsLoad(const AComboBox: TComboBox; out AIDs: TIntVector);
begin
  KeyPickLoad(AComboBox, AIDs, 'SYMBOLS', 'SymbolID', 'SymbolValue', 'SymbolID',
              False{load zero ID}, LDASH_DEFAULT{custom zero ID});
end;

end.

