PRAGMA ENCODING     = "UTF-8";
PRAGMA FOREIGN_KEYS = ON;

/* Статусы документов */
CREATE TABLE IF NOT EXISTS STATUSES (
    StatusID   INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    StatusName TEXT    NOT NULL
);
INSERT OR IGNORE INTO  STATUSES (StatusID, StatusName) VALUES (0, '<не указан>');
INSERT OR IGNORE INTO  STATUSES (StatusID, StatusName) VALUES (1, 'действует');

/* Типы документов */
CREATE TABLE IF NOT EXISTS TYPES (
    TypeID   INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    TypeName TEXT    NOT NULL
);
INSERT OR IGNORE INTO  TYPES (TypeID, TypeName) VALUES (0, '<не указан>');
INSERT OR IGNORE INTO  TYPES (TypeID, TypeName) VALUES (1, 'ГОСТ');
INSERT OR IGNORE INTO  TYPES (TypeID, TypeName) VALUES (2, 'ГОСТ Р');
INSERT OR IGNORE INTO  TYPES (TypeID, TypeName) VALUES (3, 'ГОСТ IEC');
INSERT OR IGNORE INTO  TYPES (TypeID, TypeName) VALUES (4, 'ГОСТ Р ИСО');

/* Параметры */
CREATE TABLE IF NOT EXISTS SETTINGS (
    NAME  TEXT    PRIMARY KEY NOT NULL,
    VALUE INTEGER NOT NULL
);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('FILTERTOGGLE', 1);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('DOCINFOTOGGLE', 1);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('ADDONTOGGLE', 1);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('INFOPANELHEIGHT', 400);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('SERIALNUMCOLWIDTH', 60);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('ACTUALIZATIONCOLWIDTH', 130);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('DOCTYPECOLWIDTH', 130);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('DOCNUMCOLWIDTH', 130);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('DOCDATECOLWIDTH', 130);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('STATUSCOLWIDTH', 150);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('DOCNAMECOLWIDTH', 250);
INSERT OR IGNORE INTO  SETTINGS (NAME, VALUE) VALUES ('NOTECOLWIDTH', 250);

/* Разделители номера и года */
CREATE TABLE IF NOT EXISTS SYMBOLS (
    SymbolID    INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    SymbolValue TEXT    NOT NULL
);
INSERT OR IGNORE INTO  SYMBOLS (SymbolID, SymbolValue) VALUES (0, '—');
INSERT OR IGNORE INTO  SYMBOLS (SymbolID, SymbolValue) VALUES (1, ':');
INSERT OR IGNORE INTO  SYMBOLS (SymbolID, SymbolValue) VALUES (2, '/');

/* Список документов */
CREATE TABLE IF NOT EXISTS DOCUMENTS (
    DocID       INTEGER  PRIMARY KEY AUTOINCREMENT NOT NULL,
    StatusID    INTEGER  NOT NULL DEFAULT 0,
    TypeID      INTEGER  NOT NULL DEFAULT 0,
    DocNum      TEXT     NOT NULL,
    SymbolID    INTEGER  NOT NULL DEFAULT 0,
    DocYear     TEXT     NOT NULL,
    DocName     TEXT     NOT NULL,
    DocDate     DATETIME NOT NULL,
    Note        TEXT    ,
    ControlDate DATETIME NOT NULL,
    CONSTRAINT FK_DOCUMENTS_STATUSID FOREIGN KEY (StatusID) REFERENCES STATUSES(StatusID) ON UPDATE CASCADE ON DELETE SET DEFAULT,
    CONSTRAINT FK_DOCUMENTS_TYPEID   FOREIGN KEY (TypeID)   REFERENCES TYPES(TypeID)      ON UPDATE CASCADE ON DELETE SET DEFAULT,
    CONSTRAINT FK_DOCUMENTS_SYMBOLID FOREIGN KEY (SymbolID) REFERENCES SYMBOLS(SymbolID)  ON UPDATE CASCADE ON DELETE SET DEFAULT
);

/* Поправки и изменения */
CREATE TABLE IF NOT EXISTS ADDONS (
    AddonID   INTEGER  PRIMARY KEY AUTOINCREMENT NOT NULL,
    AddonDate DATETIME NOT NULL,
    AddonName TEXT     NOT NULL,
    AddonNum  TEXT    ,
    AddonNote TEXT    ,
    DocID     INTEGER  NOT NULL,
    CONSTRAINT FK_ADDONS_DOCID FOREIGN KEY (DocID) REFERENCES DOCUMENTS(DocID) ON UPDATE CASCADE ON DELETE CASCADE
);
