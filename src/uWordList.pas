unit uWordList;

{
  Copyright (c) 2007 Jeremy Darling
  Modifications copyright (c) 2010-2014 Felipe Daragon
  
  This unit contains a TWordList class that implements a basic DAWG
  (Directed Acyclic Word Graph). While this class was not created
  explicitly for use with pLua it provides a fast word searching ability
  that we really need within a scripting engine. One of the advantages
  of this implementation are that it allows for data to be attached to a
  word or a letter within a word.
  
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I Lua.inc}

uses
  Classes, SysUtils;
  
type
{$IFDEF UNICODE}
  lwCha_r = AnsiChar;
{$ELSE}
  lwCha_r = Char;
{$ENDIF}

type
  PWordListSymbol = ^TWordListSymbol;
  PWordListSymbolArray=^TWordListSymbolArray;
  TWordListSymbolArray = Array of PWordListSymbol;
  TWordListReleaseWordData = procedure(DataPointer : Pointer) of object;
  TWordListSymbol = packed record
    c     : lwCha_r;    // What character is this?
    eow   : boolean; // Is this the last character of a word?
    data  : Pointer; // Anything
    below : TWordListSymbolArray; // Next characters in words.
  end;
  TLoadProgress = procedure(pos, count : Integer; loadingWord : AnsiString) of object;
  
  { TWordListInfo }

  TWordListInfo=class
  private
    fData : TWordListSymbol;
    fSize: Integer;
    function GetData: PWordListSymbol;
    procedure Sort(WhatNode : PWordListSymbol; Start : Integer = -1; Stop : Integer = -1);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(WhatChar : lwCha_r; ParentNode : PWordListSymbol; IsEnd : Boolean) : PWordListSymbol;
    function Exists(WhatChar : lwCha_r; ParentNode : PWordListSymbol; out IsEnd : Boolean) : Boolean; overload;
    function Exists(WhatChar : lwCha_r; ParentNode : PWordListSymbol) : PWordListSymbol; overload;
    procedure Clear(OnReleaseWordData : TWordListReleaseWordData = nil);
    property Size : Integer read fSize;
    property Data : PWordListSymbol read GetData;
  end;

  { TWordList }

  TWordList = class
  private
    FLoadProgress: TLoadProgress;
    fDict     : TWordListInfo;
    FOnReleaseWordData: TWordListReleaseWordData;
    function  GetData: PWordListSymbol;
    function  GetWordData(AWord: AnsiString): Pointer;
    function  GetWordSymbol(AWord: AnsiString): PWordListSymbol;
    function  InternalAddWord(WhatWord : AnsiString) : PWordListSymbol;
    procedure SetLoadProgress(const AValue: TLoadProgress);
    procedure DoLoadProgress(pos, count : Integer; loadingWord : AnsiString);
    procedure SetOnReleaseWordData(const AValue: TWordListReleaseWordData);
    procedure SetWordData(AWord: AnsiString; const AValue: Pointer);
    function  WordValid(WhatWord : AnsiString) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromStream(const aStream : TStream);
    procedure SaveToStream(const aStream : TStream);
    procedure LoadFromFile(const WhatFile : AnsiString);
    procedure SaveToFile(const WhatFile : AnsiString);
    
    procedure ListWordsFromLetters( WhatLetters : AnsiString; List : TStrings; MinCharacters : Integer = -1;
                                    MaxCharacters : Integer = -1 );
    
    procedure ListWords(const AList : TStrings);
    
    procedure Clear;
    procedure FlushList;
    function  AddWord(const WhatWord : AnsiString) : PWordListSymbol;
    function  WordExists(const WhatWord : AnsiString; AllowPartial : Boolean = false) : Boolean;
    property  LoadProgress: TLoadProgress read FLoadProgress write SetLoadProgress;
    property  Data  : PWordListSymbol read GetData;
    property  WordData[AWord:AnsiString] : Pointer read GetWordData write SetWordData;
    property  WordSymbol[AWord:AnsiString] : PWordListSymbol read GetWordSymbol;
    property  OnReleaseWordData : TWordListReleaseWordData read FOnReleaseWordData write SetOnReleaseWordData;
  end;

implementation

const
  WordListBufferSize = 26;

{ TWordList }

function  TWordList.InternalAddWord(WhatWord : AnsiString) : PWordListSymbol;
var
  n  : PWordListSymbol;
  pc : PAnsiChar;
begin
  pc := PAnsiChar(WhatWord+#0);
  n  := fDict.Data;
  while pc^<>#0 do
    begin
      n := fDict.Add(pc^, n, (pc+1)^=#0);
      inc(pc);
    end;
  result := n;
end;

function TWordList.GetData: PWordListSymbol;
begin
  result := fDict.Data;
end;

function TWordList.GetWordData(AWord: AnsiString): Pointer;
var
  n  : PWordListSymbol;
  pc : PAnsiChar;
begin
  result := nil;
  n  := fDict.Data;
  pc := PAnsiChar(AWord+#0);
  while (pc^<>#0) and assigned(n) do
    begin
      n := fDict.Exists(pc^, n);
      inc(pc);
    end;
  if assigned(n) then
    result := n^.data;
end;

function TWordList.GetWordSymbol(AWord: AnsiString): PWordListSymbol;
var
  n  : PWordListSymbol;
  pc : PAnsiChar;
begin
  result := nil;
  n  := fDict.Data;
  pc := PAnsiChar(AWord+#0);
  while (pc^<>#0) and assigned(n) do
    begin
      n := fDict.Exists(pc^, n);
      inc(pc);
    end;
  if assigned(n) then
    result := n;
end;

procedure TWordList.SetLoadProgress(const AValue: TLoadProgress);
begin
  if (@FLoadProgress = @AValue) then exit; // FD: 16/05/2010, AValue to @AValue
  FLoadProgress:=AValue;
end;

procedure TWordList.DoLoadProgress(pos, count: Integer; loadingWord : AnsiString);
begin
  if assigned(fLoadProgress) then
    fLoadProgress(pos, count, loadingWord);
end;

procedure TWordList.SetOnReleaseWordData(const AValue: TWordListReleaseWordData
  );
begin
  if @FOnReleaseWordData=@AValue then exit; // FD: 16/05/2010
  FOnReleaseWordData:=AValue;
end;

procedure TWordList.SetWordData(AWord: AnsiString; const AValue: Pointer);
var
  n  : PWordListSymbol;
  pc : PAnsiChar;
begin
  n  := fDict.Data;
  pc := PAnsiChar(AWord+#0);
  while (pc^<>#0) and assigned(n) do
    begin
      n := fDict.Exists(pc^, n);
      inc(pc);
    end;
  if assigned(n) then
    n^.data := AValue
  else
    InternalAddWord(AWord)^.data := AValue;
end;

function TWordList.WordValid(WhatWord: AnsiString): Boolean;
var
  pc : PAnsiChar;
begin
  result := false;
  if length(trim(string(WhatWord))) > 0 then
    begin
      result := true;
      pc := PAnsiChar(WhatWord+#0);
      while pc^<>#0 do
        begin
          if not(pc^ in ['a'..'z', 'A'..'Z']{, '0'..'9', '_', '-']}) then
            begin
              result := false;
              exit;
            end;
          inc(pc);
        end;
    end;
end;

constructor TWordList.Create;
begin
  fDict := TWordListInfo.Create;
end;

destructor TWordList.Destroy;
begin
  FlushList;
  fDict.Free;
  inherited Destroy;
end;

procedure TWordList.LoadFromStream(const aStream: TStream);
var
  aPos        : Integer;
  Word        : AnsiString;
  c           : lwCha_r;
begin
  FlushList;
  aPos := aStream.Position;
  Word := '';
  c    := #0;
  while aStream.Position < aStream.Size do
    begin
      aStream.Read(c, sizeof(c));
      case c of
        #13 : begin // end of word
                if (Word <> '') and WordValid(Word) then
                  begin
                    DoLoadProgress(aStream.Position-aPos, aStream.Size-aPos, Word);
                    InternalAddWord(Word+#0);
                  end;
                Word := '';
              end;
        #0..#8,
        #10 : ; // eat it
      else
        Word := Word + UpCase(c);
      end;
    end;
end;

procedure TWordList.SaveToStream(const aStream: TStream);
  procedure GetWords(b : AnsiString; aNode : PWordListSymbol);
  var
    i : Integer;
    s : AnsiString;
  begin
    if aNode^.eow then
      begin
        s := b + #13#10;
        aStream.Write(s[1], length(s[1]));
      end;
    for i := 0 to length(aNode^.below)-1 do
      GetWords(b + aNode^.below[i]^.c, aNode^.below[i]);
  end;
begin
  GetWords('', fDict.Data);
  raise Exception.Create('TWordList.SaveToStream not yet implemented!')
end;

procedure TWordList.LoadFromFile(const WhatFile: AnsiString);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(string(WhatFile), fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TWordList.SaveToFile(const WhatFile: AnsiString);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(string(WhatFile), fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TWordList.ListWordsFromLetters(WhatLetters: AnsiString; List : TStrings;
  MinCharacters: Integer; MaxCharacters: Integer);
var
  s : AnsiString;
  i,
  c : Integer;
  n : PWordListSymbol;
  procedure CheckAdd(WhatStr : AnsiString);
  var
    bAdd : Boolean;
  begin
    bAdd := true;
    if (MinCharacters > -1) then
      bAdd := length(WhatStr) >= MinCharacters;
    if (MaxCharacters > -1) then
      bAdd := bAdd and (length(WhatStr) <= MaxCharacters);
    if bAdd and (List.IndexOf(string(WhatStr))=-1) then
      List.Add(string(WhatStr));
  end;
  procedure TestThis(Base, Letters : AnsiString; Node : PWordListSymbol);
  var
    j    : Integer;
    tn   : PWordListSymbol;
    ts   : AnsiString;
  begin
    j := 1;
    while j <= Length(Letters) do
      begin
        tn := fDict.Exists(Letters[j], Node);
        if assigned(tn) then
          begin
            if tn^.eow then
              CheckAdd(base + letters[j]);
            ts := Letters;
            delete(ts, j, 1);
            TestThis(base + letters[j], ts, tn);
          end;
        inc(j);
      end;
  end;
begin
  i := 1;
  c := Length(WhatLetters);
  while i <= c do
    begin
      n := fDict.Exists(WhatLetters[i], fDict.Data);
      if n <> nil then
        begin
          s := WhatLetters;
          delete(s, i, 1);
          if n^.eow then
            CheckAdd(WhatLetters[i]);
          TestThis(WhatLetters[i], s, n);
        end;
      inc(i);
    end;
end;

procedure TWordList.ListWords(const AList: TStrings);
var
  ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveToStream(ms);
    AList.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TWordList.Clear;
begin
  FlushList;
end;

procedure TWordList.FlushList;
begin
  fDict.Clear(FOnReleaseWordData);
end;

function TWordList.AddWord(const WhatWord: AnsiString) : PWordListSymbol;
var
  aw : AnsiString;
begin
  //result := nil;
  aw := ansistring(Trim(string(WhatWord)))+#0;
  {if not WordValid(aw) then
    exit;
  if not WordExists(aw) then}
    result := InternalAddWord(aw);
end;

function TWordList.WordExists(const WhatWord: AnsiString; AllowPartial : Boolean): Boolean;
var
  n  : PWordListSymbol;
  pc : PAnsiChar;
begin
  n  := fDict.Data;
  pc := PAnsiChar(WhatWord+#0);
  while (pc^<>#0) and assigned(n) do
    begin
      n := fDict.Exists(pc^, n);
      inc(pc);
    end;
  result := assigned(n) and (n^.eow or AllowPartial);
end;

{ TWordListInfo }

function TWordListInfo.GetData: PWordListSymbol;
begin
  result := @fData;
end;

procedure TWordListInfo.Sort(WhatNode: PWordListSymbol; Start: Integer;
  Stop: Integer);
  procedure iSort(var r : TWordListSymbolArray; lo, up : integer );
  var
    i, j : Integer;
    tempr: PWordListSymbol;
  begin
    while up>lo do
      begin
        i := lo;
        j := up;
        tempr := r[lo];
        {*** Split file in two ***}
        while i<j do
          begin
            while r[j]^.c > tempr^.c do
              j := j-1; r[i] := r[j];
            while (i<j) and (r[i]^.c<=tempr^.c) do
              i := i+1;
            r[j] := r[i];
          end;
        r[i] := tempr;
        {*** Sort recursively ***}
        iSort(r,lo,i-1);
        lo := i+1
      end
  end;
begin
  if Start = -1 then
    Start := low(WhatNode^.below);
  if Stop = -1 then
    Stop := high(WhatNode^.below);
  if (Start = Stop) or (Start=-1) or (Stop=-1) then
    exit;
  iSort(WhatNode^.below, Start, Stop);
end;

constructor TWordListInfo.Create;
begin
end;

destructor TWordListInfo.Destroy;
begin
  Clear;
  inherited;
end;

function TWordListInfo.Add(WhatChar: lwCha_r; ParentNode: PWordListSymbol;
  IsEnd: Boolean): PWordListSymbol;
begin
  WhatChar := UpCase(WhatChar);
  result := Exists(WhatChar, ParentNode);
  if assigned(result) then
    result^.eow := result^.eow or IsEnd
  else
    begin
      new(result);
      result^.c := WhatChar;
      result^.eow := IsEnd;
      result^.data := nil;
      SetLength(ParentNode^.below, length(ParentNode^.below)+1);
      ParentNode^.below[length(ParentNode^.below)-1] := result;
      Sort(ParentNode);
    end;
end;

function TWordListInfo.Exists(WhatChar: lwCha_r; ParentNode: PWordListSymbol;
  out IsEnd: Boolean): Boolean;
var
  n : PWordListSymbol;
begin
  IsEnd := false;
  n := Exists(WhatChar, ParentNode);
  result := assigned(n);
  if result then
    IsEnd := n^.eow;
end;

function TWordListInfo.Exists(WhatChar: lwCha_r; ParentNode: PWordListSymbol
  ): PWordListSymbol;
var
  f, l, j : Integer;
  c : lwCha_r;
begin
  result := nil;
  f := 0;
  l := length(ParentNode^.below)-1;
  WhatChar := UpCase(WhatChar);
  if l = -1 then
    begin
      result := nil;
      exit;
    end;
  while (l-f) > 1 do
    begin
      j := (l+f) div 2;
      c := ParentNode^.below[j]^.c;
      if WhatChar <= c then
        l := j
      else
        f := j;
    end;
  c := ParentNode^.below[l]^.c;
  if c = WhatChar then
    result := ParentNode^.below[l]
  else if (l <> f) and (f>=0) then
    begin
      c := ParentNode^.below[f]^.c;
      if c = WhatChar then
        result := ParentNode^.below[f]
    end;
end;

procedure TWordListInfo.Clear(OnReleaseWordData : TWordListReleaseWordData = nil);
var
  i : Integer;
  procedure ClearNode(WhatNode : PWordListSymbol);
  var
    c : Integer;
  begin
    if assigned(OnReleaseWordData) and WhatNode^.eow and assigned(WhatNode^.data) then
      OnReleaseWordData(WhatNode^.data);
    for c := length(WhatNode^.below)-1 downto 0 do
      ClearNode(WhatNode^.below[c]);
    SetLength(WhatNode^.below, 0);
    Freemem(WhatNode);
  end;
begin
  if Length(fData.below) = 0 then
    exit;
  for i := 0 to Length(fData.below)-1 do
    ClearNode(fData.below[i]);
  SetLength(fData.below, 0);
end;

end.

