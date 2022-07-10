unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ExtCtrls, CheckLst;

type

  { TfrmMainMenu }

  TfrmMainMenu = class(TForm)
    btnSearch: TButton;
    btnParse: TButton;
    btnSettings: TButton;
    childAbsPos: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    Label1: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    myFilesList: TCheckListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    RightPanel: TPanel;
    LeftPanel: TPanel;
    procedure btnSearchClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure ListFileDir(Path: string);
    procedure Log(str: String);
    procedure setStatus(str: String);
    procedure parseFile(fileName: String);
    procedure clearObjects;
  private

  public

  end;

  S_myObject = record
    name: String;
    left: Integer;
    top: Integer;
    height: Integer;
    width: Integer;
    objType: String;
    text: String;
    OnClick: String;
    enabled: Boolean;
    FontHeight: Integer;
    PictureData: String;
  end;

var
  frmMainMenu: TfrmMainMenu;
  directories, files, ignored: Integer;
  myObjects: Array [0..50] of S_myObject;
  levels: Array[0..10] of Integer;
  levelIndex: Integer;
  totalObjects: Integer;

implementation

{$R *.lfm}

{ TfrmMainMenu }

procedure TfrmMainMenu.setStatus(str: String);
begin
     Panel1.Caption := str;
end;

procedure TfrmMainMenu.Log(str: String);
begin
     Memo1.Lines.add(str);
end;

procedure TfrmMainMenu.btnSearchClick(Sender: TObject);
begin
  setStatus('Searching...');

  DirectoryEdit1.Enabled := false;
  btnSearch.Enabled := false;
  btnParse.Enabled := false;

  myFilesList.Clear;
  directories := 0;
  files := 0;
  ignored := 0;

  ListFileDir(DirectoryEdit1.Directory + '\');
  //myFilesList.CheckAll(cbChecked, 0);

  Log('Operation completed. Scanned ' +
                 IntToStr(directories) + ' directories and found ' +
                 IntToStr(files) + ' files (' +
                 IntToStr(ignored) + ' files ignored)');

  DirectoryEdit1.Enabled := true;
  btnSearch.Enabled := true;
  if (files > 0) then
    btnParse.Enabled := true;

  setStatus('Operation completed');
end;

procedure TfrmMainMenu.clearObjects;
var
  x : Integer;
begin
  totalObjects := -1;
  levelIndex := -1;
  for x := 0 to 49 do
      begin
        myObjects[x].name := '';
        myObjects[x].objType := '';
        myObjects[x].left := -1;
        myObjects[x].top := -1;
        myObjects[x].height := -1;
        myObjects[x].width := -1;
        myObjects[x].text := '';
        myObjects[x].OnClick := '';
        myObjects[x].enabled := true;
        myObjects[x].FontHeight := 0;
        myObjects[x].PictureData := '';
      end;
  for x := 0 to 9 do
      levels[x] := 0;
end;

procedure TfrmMainMenu.parseFile(fileName: String);
var txtFile: TextFile;
    line : String;
    x : Integer;
    checked, errors : Integer;
    ignoreLines : Boolean;
    PictureData : boolean;
begin
  clearObjects;
  ignoreLines := false;

  setStatus('Parsing: ' + fileName);
  Log('Parsing: ' + fileName);
  AssignFile(txtFile, fileName);
  try
    Reset(txtFile);
    while not Eof(txtFile) do
      begin
        ReadLn(txtFile, line);
        line := Trim(line);
        //Log(line);

        if line.Equals('Picture.Data = {') then
          begin
            PictureData := true;
            continue;
          end;
        if line.equals('}') And PictureData = true then
          begin
            PictureData := false;
            continue;
          end;
        if PictureData = true then
          begin
            myObjects[totalObjects].PictureData :=
              myObjects[totalObjects].PictureData + line;
            continue;
          end;

        if (line.Equals('Lines.Strings = (')) then
          ignoreLines := true
        else if (ignoreLines = true) And (line.Equals(')')) then
          begin
            ignoreLines := false;
            continue;
          end;

        if ignoreLines = false then
          begin
            if (line.Equals('Enabled = False')) then
              begin
                myObjects[totalObjects].enabled := false;
                Log('Object will ignored');
              end
            else if (line.startsWith('object')) then
              begin
                Inc(totalObjects);
                Inc(levelIndex);
                levels[levelIndex] := totalObjects;

                delete(line, 1, 7);
                x := pos(':', line) - 1;
                myObjects[totalObjects].name := Copy(line, 1, x);
                delete(line, 1, x+2);
                myObjects[totalObjects].objType := line;
                Log('Creating new "' + myObjects[totalObjects].objType + '" object with name: "' + myObjects[totalObjects].name + '"');
              end
            else if (line.startsWith('end')) then
              begin
                //Log('Object ended: ' + myObjects[levels[levelIndex]].name);
                Dec(levelIndex);
                //if (levelIndex > -1) then
                //   Log('Returning to object: ' + myObjects[levels[levelIndex]].name);
              end
            else if (line.startsWith('Left')) then
              begin
                if totalObjects = 0 then
                  myObjects[totalObjects].left := 0
                else
                  begin
                    myObjects[totalObjects].left := StrToInt(Copy(line, 8, length(line)-7));
                    if childAbsPos.Checked And (levelIndex > 0) then
                      begin
                          myObjects[totalObjects].left :=
                             myObjects[totalObjects].left +
                             myObjects[levels[levelIndex-1]].left;
                      end;
                  end;
              end
            else if (line.startsWith('Top')) then
              begin
                if totalObjects = 0 then
                    myObjects[totalObjects].top := 0
                else
                  begin
                    myObjects[totalObjects].top := StrToInt(Copy(line, 7, length(line)-6));
                    if childAbsPos.Checked And (levelIndex > 0) then
                      begin
                          myObjects[totalObjects].top :=
                             myObjects[totalObjects].top +
                             myObjects[levels[levelIndex-1]].top;
                      end;
                  end;
              end
            else if (line.startsWith('Height')) then
              myObjects[totalObjects].height := StrToInt(Copy(line, 10, length(line)-9))
            else if (line.startsWith('Width')) then
              myObjects[totalObjects].width := StrToInt(Copy(line, 9, length(line)-8))
            else if (line.startsWith('Caption')) then
              myObjects[totalObjects].text := Copy(line, 11, length(line)-10)
            else if (line.startsWith('OnClick')) then
              myObjects[totalObjects].onClick := Copy(line, 11, length(line)-10)
            else if (line.startsWith('Font.Height')) then
              myObjects[totalObjects].FontHeight := StrToInt(Copy(line, 15, length(line)-14))

            //BorderSpacing.Right
          end;
      end;
  finally
    CloseFile(txtFile);
  end;

  Log(IntToStr(TotalObjects) + ' objects found.');

  Log('Checking objects...');
  checked :=0;
  errors := 0;

  for x := 0 to TotalObjects - 1 do
      begin
        if (myObjects[x].enabled) then
          begin
            Inc(Checked);
            Log('Checking object #' + IntToStr(x+1) + ' "' + myObjects[x].name + '"...');
            if length(myObjects[x].name) = 0 then
              begin
                Log('Object name is missing');
                Inc(errors);
              end;
            if length(myObjects[x].objType) = 0 then
              begin
                Log('Object type is missing');
                Inc(errors);
              end;
            if myObjects[x].left = -1 then
              begin
                Log('Property "Left" is missing');
                Inc(errors);
              end;
            if myObjects[x].top = -1 then
              begin
                Log('Property "Top" is missing');
                Inc(errors);
              end;
            if myObjects[x].height = -1 then
              begin
                Log('Property "Height" is missing');
                Inc(errors);
              end;
            if myObjects[x].width = -1 then
              begin
                Log('Property "Width" is missing');
                Inc(errors);
              end;
            if length(myObjects[x].PictureData) > 0 then
              //Log(myObjects[x].PictureData);
              Log('Object contains ' + IntToStr(length(myObjects[x].PictureData)) + ' bytes of "PictureData"');
          end;
      end;
  Log('Total objectets checked: ' + IntToStr(checked));
  Log('Total errors: ' + IntToStr(errors));

  filename := ChangeFileExt(filename, '.ino');
  Log('Generating output file ' + filename);

end;

procedure TfrmMainMenu.btnParseClick(Sender: TObject);
var i : Integer;
begin

  Log('Parsing started');
  DirectoryEdit1.Enabled := false;
  btnSearch.Enabled := false;
  btnParse.Enabled := false;
  for i := 0 to myFilesList.Count-1 do
      if myFilesList.Checked[i] then
         parseFile(myFilesList.Items.Strings[i]);
  DirectoryEdit1.Enabled := true;
  btnSearch.Enabled := true;
  btnParse.Enabled := true;

  setStatus('Operation completed');
end;

procedure TfrmMainMenu.btnSettingsClick(Sender: TObject);
begin
  RightPanel.Visible := not RightPanel.Visible;
  if (RightPanel.Visible = true) then
     btnSettings.Caption:='Settings <'
  else
    btnSettings.Caption:='Settings >'

end;

procedure TfrmMainMenu.DirectoryEdit1AcceptDirectory(Sender: TObject;
  var Value: String);
begin
  myFilesList.clear();
end;

procedure TfrmMainMenu.ListFileDir(Path: string);
  var
    SR: TSearchRec;
  begin
    Inc(directories);
    Log('Searching directory: ' + Path);
    if FindFirst(Path + '*.*', faAnyFile, SR) = 0 then
    begin
      repeat
        if (SR.Attr <> faDirectory) then
           begin
             if copy(SR.Name, length(SR.Name)-3, 4) = '.lfm' then
                begin
                  Inc(files);
                  Log('Adding file: ' + SR.Name);
                  myFilesList.Items.Add(Path + SR.Name);
                end
             else
               Inc(ignored);
           end
        else
            begin
                if (SR.Name <> '.') and (SR.Name <> '..') then
                   ListFileDir(Path + SR.Name + '\');
            end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;

procedure TfrmMainMenu.FormCreate(Sender: TObject);
begin
  DirectoryEdit1.Directory := GetCurrentDir();
  btnParse.Enabled := false;
  Memo1.Lines.Clear;
  setStatus('Ready');
end;

end.

