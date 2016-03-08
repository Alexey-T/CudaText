(**
*                               TCOPYDIR
*                               ========
*                                 v2.1
*
*  This unit contains "TCopyDir" class wich copies entire directories (incl. its
*  subdirectories)
*
*  Author: bastla (@ Supernature-Forum / @ Lazarus Forum)
*  Contact: send me a PM via Lazarus Forum
*      (http://forum.lazarus.freepascal.org/index.php?action=pm;sa=send;u=49100)
*  License: Free Domain
*  Website: http://forum.lazarus.freepascal.org/index.php/topic,20759.0.html
*
*  How TCopyDir works:
*  -------------------
*  TCopyDir uses TFileSearcher to enumerate a whole directory and copies its
*  content file by file.
*  Because of using LCL-components only, this class should work on all available
*  platforms supported by Lazarus and LCL.
*
*  Disclaimer:
*  -----------
*  THIS SOFTWARE IS PROVIDED BY BASTLA "AS IS" AND ANY
*  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL BASTLA BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
*  History:
*  --------
*  v1:
*     - initial release
*  v2:
*     - added support for file attributes (not for Unix based systems)
*     - improved whole code (it should be much easier to modify the code now)
*  v2.1:
*     - fixed a bug causing directories containg no subfolders not being copied
*       (affecting lines 181-182) -- SPECIAL THANKS TO onivan!
*     - fixed a bug causing an error on Unix machines (affecting line 292) --
*       SPECIAL THANKS TO Caladan!
*
*
*  Greetings,
*  bastla
*)

unit CopyDir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils, FIleUtil, LCLIntf;

type
  TList = array of String;
  TCopyDir = class
  private
    _dirSource, _dirTarget: String;

    _fs: TFileSearcher;
    _log: TStringList;

    _files: TList;
    _directories: TList;

    _copied: Boolean;
    _dirsCreated: Boolean;
    _enumerated: Boolean;

    _copyingDone: TList;
    _copyingFailed: TList;
    _creatingDone: TList;
    _creatingFailed: TList;

    _preserveFileDates: Boolean;
    _preserveAttributes: Boolean;

    _copyReadOnly: Boolean;
    _copyHidden: Boolean;
    _copySystem: Boolean;
    _copyArchive: Boolean;

    _printToTerminal: Boolean;

    procedure _AddToList(aString: String; var aList: TList);
    procedure _AddToLog(aNote: String);
    procedure _CopyFile(aFile: String);
    procedure _CopyFiles;
    procedure _CreateDir(aDir: String);
    procedure _CreateDirs;
    procedure _DirFound(FileIterator: TFileIterator);
    procedure _FileFound(FileIterator: TFileIterator);

    function _CanCopy(aFile: String): Boolean;
    function _SourceToTarget(aTarget: String): String;
  public
    constructor Create(aSourceDir, aTargetDir: String);
    destructor Destroy;

    property PreserverFileDates: Boolean
      read _preserveFileDates
      write _preserveFileDates;
    {$IFNDEF Unix} property PreserveAttributes: Boolean
      read _preserveAttributes
      write _preserveAttributes; {$ENDIF}

    property CopyReadOnlyFiles: Boolean
      read _copyReadOnly
      write _copyReadOnly;
    property CopyHiddenFiles: Boolean
      read _copyHidden
      write _copyHidden;
    property CopySystemFiles: Boolean
      read _copySystem
      write _copySystem;
    property CopyArchiveFiles: Boolean
      read _copyArchive
      write _copyArchive;

    property PrintToTerminal: Boolean
      read _printToTerminal
      write _printToTerminal;

    procedure Start;
    procedure Enumerate;

    function GetLog: TStringList;
  end;

implementation

// ************************************************************************** \\
// ***************************** PRIVATE SECTION **************************** \\
// ************************************************************************** \\

procedure TCopyDir._AddToList(aString: String; var aList: TList);
begin
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aString;
end;

procedure TCopyDir._AddToLog(aNote: String);
begin
  self._log.Append(aNote);
  if self._printToTerminal then WriteLn(aNote);
end;

procedure TCopyDir._CopyFile(aFile: String);
begin
  if CopyFile(aFile, self._SourceToTarget(aFile), self._preserveFileDates) then
  begin
    if self._preserveAttributes then
    begin
      if FileSetAttrUTF8(self._SourceToTarget(aFile),
        FileGetAttrUTF8(aFile)) <> -1 then
      begin
        self._AddToList(aFile, self._copyingDone);
      end;
    end else
    begin
      self._AddToList(aFile, self._copyingDone);
    end;
  end else
  begin
    self._AddToList(aFile, self._copyingFailed);
  end;
end;

procedure TCopyDir._CopyFiles;
var
  i: LongWord;
begin
  if (self._enumerated) and ((self._dirsCreated) or
    (Length(self._directories) = 0)) and (Length(self._files) > 0) then
  begin
    for i := Low(self._files) to High(self._files) do
    begin
      if self._files[i] <> '' then
      begin
        self._CopyFile(self._files[i]);
      end;
    end;

    self._copied := true;
  end;
end;

procedure TCopyDir._CreateDir(aDir: String);
begin
  if ForceDirectoriesUTF8(aDir) then
  begin
    self._AddToList(aDir, self._creatingDone);
  end else
  begin
    self._AddToList(aDir, self._creatingFailed);
  end;
end;

procedure TCopyDir._CreateDirs;
var
  i: LongWord;
begin
  if (self._enumerated) and (Length(self._directories) > 0) then
  begin
    for i := Low(self._directories) to High(self._directories) do
    begin
      if self._directories[i] <> '' then
      begin
        self._CreateDir(self._SourceToTarget(self._directories[i]));
      end;
    end;

    self._dirsCreated := true;
  end;
end;

procedure TCopyDir._DirFound(FileIterator: TFileIterator);
begin
  self._AddToList(FileIterator.FileName, self._directories);
end;

procedure TCopyDir._FileFound(FileIterator: TFileIterator);
begin
  if self._CanCopy(FileIterator.FileName) then
  begin
    self._AddToList(FileIterator.FileName, self._files);
  end;
end;

function TCopyDir._CanCopy(aFile: String): Boolean;
var
  __fileAttributes: LongInt;
begin
  __fileAttributes := FileGetAttrUTF8(aFile);

  if (__fileAttributes and faReadOnly <> 0) and not self._copyReadOnly then
  begin
    Result := false;
    Exit;
  end;
  if (__fileAttributes and faHidden <> 0) and not self._copyHidden then
  begin
    Result := false;
    Exit;
  end;
  if (__fileAttributes and faSysFile <> 0) and not self._copySystem then
  begin
    Result := false;
    Exit;
  end;
  if (__fileAttributes and faArchive <> 0) and not self._copyArchive then
  begin
    Result := false;
    Exit;
  end;

  Result := true;
end;

function TCopyDir._SourceToTarget(aTarget: String): String;
begin
  Result := IncludeTrailingPathDelimiter(self._dirTarget) + Copy(aTarget,
    Length(self._dirSource) + 1, Length(aTarget));
end;


// ************************************************************************** \\
// ***************************** PUBLIC SECTION ***************************** \\
// ************************************************************************** \\

constructor TCopyDir.Create(aSourceDir, aTargetDir: String);
begin
  self._fs := TFileSearcher.Create;
  self._fs.OnDirectoryFound := @self._DirFound;
  self._fs.OnFileFound := @self._FileFound;
  self._log := TStringList.Create;

  self._copied := false;
  self._dirsCreated := false;
  self._enumerated := false;

  self._preserveFileDates := true;

  {$IFDEF Unix} self._preserveAttributes := false;
    {$ELSE} self._preserveAttributes := true {$ENDIF};
  self._copyReadOnly := true;
  self._copyHidden := true;
  self._copySystem := true;
  self._copyArchive := true;

  self._printToTerminal := false;


  self._AddToLog('Initializing...');

  self._dirSource := aSourceDir;
  if DirectoryExistsUTF8(self._dirSource) then
  begin
    self._AddToLog('Source: ' + self._dirSource + ' - exists!');
  end else
  begin
    if ForceDirectoriesUTF8(self._dirSource) then
    begin
      self._AddToLog('Source: ' + self._dirSource + ' - doesn''t exist, ' +
        'created!');
    end else
    begin
      self._AddToLog('Source: ' + self._dirSource + ' - doesn''t exist, ' +
        'creating failed!');
    end;
  end;

  self._dirTarget := aTargetDir;
  if DirectoryExistsUTF8(self._dirTarget) then
  begin
    self._AddToLog('Target: ' + self._dirTarget + ' - exists!');
  end else
  begin
    if ForceDirectoriesUTF8(self._dirTarget) then
    begin
      self._AddToLog('Target: ' + self._dirTarget + ' - doesn''t exist, ' +
        'created!');
    end else
    begin
      self._AddToLog('Target: ' + self._dirTarget + ' - doesn''t exist, ' +
        'creating failed!');
    end;
  end;

  self._AddToLog('');
end;

destructor TCopyDir.Destroy;
begin
  self._fs.Free;
  self._log.Free;
end;

procedure TCopyDir.Start;
var
  __startTime: DWord;
  i: Integer;
begin
  if not self._enumerated then self.Enumerate;

  __startTime := GetTickCount;
  self._AddToLog('');
  self._AddToLog('COPYING STARTED');
  self._AddToLog('===========================================================');
  if self._preserveFileDates and self._preserveAttributes then
  begin
    self._AddToLog('(Preserving dates and attributes)');
  end;
  if self._preserveFileDates and not self._preserveAttributes then
  begin
    self._AddToLog('(Preserving dates)');
  end;
  if not self._preserveFileDates and self._preserveAttributes then
  begin
    self._AddToLog('(Preserving attributes)');
  end;
  self._AddToLog('');


  self._AddToLog('1. Creating directory structure...');
  self._AddToLog('----------------------------------');
  self._AddToLog('');

  self._CreateDirs;

  self._AddToLog(IntToStr(Length(self._creatingDone)) + ' directories ' +
    'successfully created:');
  for i := Low(self._creatingDone) to High(self._creatingDone) do
  begin
    self._AddToLog(self._creatingDone[i]);
  end;
  self._AddToLog('');
  self._AddToLog(IntToStr(Length(self._creatingFailed)) + ' directories ' +
    'failed creating:');
  for i := Low(self._creatingFailed) to High(self._creatingFailed)do
  begin
    self._AddToLog(self._creatingFailed[i]);
  end;
  self._AddToLog('');


  self._AddToLog('2. Copying files...');
  self._AddToLog('-------------------');
  self._AddToLog('');

  self._CopyFiles;

  self._AddToLog(IntToStr(Length(self._copyingDone)) + ' files ' +
    'successfully copied:');
  for i := Low(self._copyingDone) to High(self._copyingDone) do
  begin
    self._AddToLog(self._copyingDone[i]);
  end;
  self._AddToLog('');
  self._AddToLog(IntToStr(Length(self._copyingFailed)) + ' files failed ' +
    'copying:');
  for i := Low(self._copyingFailed) to High(self._copyingFailed) do
  begin
    self._AddToLog(self._copyingFailed[i]);
  end;

  self._AddToLog('');
  self._AddToLog('Overall ' + IntToStr(Length(self._copyingDone)) + ' files ' +
    'in ' + IntToStr(Length(self._creatingDone)) + ' directories copied!');
  self._AddToLog('===========================================================');
  self._AddToLog('COPYING DONE (in ' + IntToStr(GetTickCount - __startTime) +
    ' ms)');
  self._AddToLog('');
end;

procedure TCopyDir.Enumerate;
begin
  self._AddToLog('Enumerating source directory...');
  if self._copyReadOnly then self._AddToLog('Including files with "ReadOnly" ' +
    'attribute');
  if self._copyHidden then self._AddToLog('Including files with "Hidden" ' +
    'attribute');
  if self._copySystem then self._AddToLog('Including files with "System" ' +
    'attribute');
  if self._copyArchive then self._AddToLog('Including files with "Archive" ' +
    'attribute');

  self._fs.Search(self._dirSource, '', true);

  self._AddToLog('Found ' + IntToStr(Length(self._files)) + ' files to copy ' +
    'in ' + IntToStr(Length(self._directories)) + ' directories.');
  self._enumerated := true;
  self._AddToLog('');
end;

function TCopyDir.GetLog: TStringList;
begin
  self._log.Append('');
  self._log.Append('Greetings,');
  self._log.Append('bastla (@ Lazarus Forum)');
  Result := self._log;
end;

end.

