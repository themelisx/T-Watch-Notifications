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
    searchRecursively: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
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
    FontColor: LongInt;
    BackColor: LongInt;
    PictureData: String;
    Visible: String;
  end;

  S_Screen = record
    count: Integer;
    myObjects: Array [0..50] of S_myObject;
  end;

var
  frmMainMenu: TfrmMainMenu;
  directories, files, ignored: Integer;
  myScreens: Array [0..50] of S_Screen;
  levels: Array[0..10] of Integer;
  levelIndex: Integer;
  screenIndex: Integer;
  maxObjectsInScreen : Integer;
  checked, errors : Integer;

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
  myFilesList.CheckAll(cbChecked);

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
  i, x : Integer;
begin
  screenIndex := 0;
  maxObjectsInScreen := 0;

  for i := 0 to 49 do
    for x := 0 to 49 do
        begin
          myScreens[i].myObjects[x].Name := '';
          myScreens[i].myObjects[x].ObjType := '';
          myScreens[i].myObjects[x].Left := -1;
          myScreens[i].myObjects[x].Top := -1;
          myScreens[i].myObjects[x].Height := -1;
          myScreens[i].myObjects[x].Width := -1;
          myScreens[i].myObjects[x].Text := '';
          myScreens[i].myObjects[x].OnClick := '';
          myScreens[i].myObjects[x].Enabled := true;
          myScreens[i].myObjects[x].Visible := 'true';
          myScreens[i].myObjects[x].FontHeight := 0;
          myScreens[i].myObjects[x].FontColor := -1;
          myScreens[i].myObjects[x].BackColor := -1;
          myScreens[i].myObjects[x].PictureData := '';
        end;
end;

procedure TfrmMainMenu.parseFile(fileName: String);
var txtFile: TextFile;
    line : String;
    x : Integer;
    ignoreLines : Boolean;
    PictureData : boolean;
    totalObjects: Integer;
begin
  ignoreLines := false;
  PictureData := false;

  totalObjects := -1;
  levelIndex := 0;

  for x := 0 to 9 do
      levels[x] := 0;

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
            myScreens[screenIndex].myObjects[totalObjects].PictureData :=
              myScreens[screenIndex].myObjects[totalObjects].PictureData + line;
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
                myScreens[screenIndex].myObjects[totalObjects].enabled := false;
                Log('Object will ignored');
              end
            else if (line.startsWith('object')) then
              begin
                Inc(totalObjects);
                Inc(levelIndex);
                levels[levelIndex] := totalObjects;

                delete(line, 1, 7);
                x := pos(':', line) - 1;
                myScreens[screenIndex].myObjects[totalObjects].name := Copy(line, 1, x);
                delete(line, 1, x+2);
                myScreens[screenIndex].myObjects[totalObjects].objType := line;
                Log('Creating new "' +
                              myScreens[screenIndex].myObjects[totalObjects].objType +
                              '" object with name: "' +
                              myScreens[screenIndex].myObjects[totalObjects].name + '"');
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
                  myScreens[screenIndex].myObjects[totalObjects].left := 0
                else
                  begin
                    myScreens[screenIndex].myObjects[totalObjects].left := StrToInt(Copy(line, 8, length(line)-7));
                    if myScreens[screenIndex].myObjects[totalObjects].left < 0 then
                      Log('[Warning] ' + myScreens[screenIndex].myObjects[totalObjects].name +
                                     ' has "Left" value: ' +
                                     IntToStr(myScreens[screenIndex].myObjects[totalObjects].left));
                    if childAbsPos.Checked And (levelIndex > 0) then
                      begin
                          myScreens[screenIndex].myObjects[totalObjects].left :=
                             myScreens[screenIndex].myObjects[totalObjects].left +
                             myScreens[screenIndex].myObjects[levels[levelIndex-1]].left;
                      end;
                  end;
              end
            else if (line.startsWith('Top')) then
              begin
                if totalObjects = 0 then
                    myScreens[screenIndex].myObjects[totalObjects].top := 0
                else
                  begin
                    myScreens[screenIndex].myObjects[totalObjects].top := StrToInt(Copy(line, 7, length(line)-6));
                    if myScreens[screenIndex].myObjects[totalObjects].top < 0 then
                      Log('[Warning] ' + myScreens[screenIndex].myObjects[totalObjects].name +
                                     ' has "Top" value: ' +
                                     IntToStr(myScreens[screenIndex].myObjects[totalObjects].top));
                    if childAbsPos.Checked And (levelIndex > 0) then
                      begin
                          myScreens[screenIndex].myObjects[totalObjects].top :=
                             myScreens[screenIndex].myObjects[totalObjects].top +
                             myScreens[screenIndex].myObjects[levels[levelIndex-1]].top;
                      end;
                  end;
              end
            else if (line.startsWith('Height')) then
              myScreens[screenIndex].myObjects[totalObjects].height := StrToInt(Copy(line, 10, length(line)-9))
            else if (line.startsWith('Width')) then
              myScreens[screenIndex].myObjects[totalObjects].width := StrToInt(Copy(line, 9, length(line)-8))
            else if (line.startsWith('Caption')) then
              myScreens[screenIndex].myObjects[totalObjects].text := Copy(line, 12, length(line)-12)
            else if (line.startsWith('OnClick')) then
              myScreens[screenIndex].myObjects[totalObjects].onClick := Copy(line, 11, length(line)-10)
            else if (line.startsWith('Font.Height')) then
              myScreens[screenIndex].myObjects[totalObjects].FontHeight := StrToInt(Copy(line, 15, length(line)-14))
            else if (line.startsWith('Font.Color')) then
              myScreens[screenIndex].myObjects[totalObjects].FontColor := StrToInt(Copy(line, 14, length(line)-13))
            else if (line.startsWith('Color')) then
              myScreens[screenIndex].myObjects[totalObjects].BackColor := StrToInt(Copy(line, 9, length(line)-8))
            else if (line.startsWith('Visible')) then
              myScreens[screenIndex].myObjects[totalObjects].Visible := LowerCase(Copy(line, 11, length(line)-10));
            //Font.Color
            //Color

            //BorderSpacing.Right
          end;
      end;
  finally
    CloseFile(txtFile);
  end;

  if TotalObjects > maxObjectsInScreen then
    maxObjectsInScreen := TotalObjects;

  myScreens[screenIndex].count := TotalObjects;

  Log(IntToStr(TotalObjects) + ' objects found.');

  Log('Checking objects...');

  for x := 0 to TotalObjects - 1 do
      begin
        if (myScreens[screenIndex].myObjects[x].enabled) then
          begin
            Inc(Checked);
            Log('Checking object #' + IntToStr(x+1) + ' "' + myScreens[screenIndex].myObjects[x].name + '"...');
            if length(myScreens[screenIndex].myObjects[x].name) = 0 then
              begin
                Log('Object name is missing');
                Inc(errors);
              end;
            if length(myScreens[screenIndex].myObjects[x].objType) = 0 then
              begin
                Log('Object type is missing');
                Inc(errors);
              end;
            if myScreens[screenIndex].myObjects[x].left = -1 then
              begin
                Log('Property "Left" is missing');
                Inc(errors);
              end;
            if myScreens[screenIndex].myObjects[x].top = -1 then
              begin
                Log('Property "Top" is missing');
                Inc(errors);
              end;
            if myScreens[screenIndex].myObjects[x].height = -1 then
              begin
                Log('Property "Height" is missing');
                Inc(errors);
              end;
            if myScreens[screenIndex].myObjects[x].width = -1 then
              begin
                Log('Property "Width" is missing');
                Inc(errors);
              end;
            if length(myScreens[screenIndex].myObjects[x].PictureData) > 0 then
              //Log(myScreens[screenIndex].myObjects[x].PictureData);
              Log('Object contains ' + IntToStr(length(myScreens[screenIndex].myObjects[x].PictureData)) + ' bytes of "PictureData"');
          end;
      end;
  Log('Total objectets checked: ' + IntToStr(checked));
  Log('Errors: ' + IntToStr(errors));

  Inc(screenIndex);
end;

procedure TfrmMainMenu.btnParseClick(Sender: TObject);
var i, j, x, total, len, numbersPerLine : Integer;
    txtFile: TextFile;
    fileName, s: String;
begin

  Log('Parsing started');
  DirectoryEdit1.Enabled := false;
  btnSearch.Enabled := false;
  btnParse.Enabled := false;
  clearObjects;
  checked := 0;
  errors := 0;

  for i := 0 to myFilesList.Count-1 do
      if myFilesList.Checked[i] then
         parseFile(myFilesList.Items.Strings[i]);

  if errors > 0 then
     begin
       Log(IntToStr(errors) + ' errors found while parsing.');
       Log('Operation aborted.');
       setStatus('Operation aborted.');
       abort;
     end;

  filename := DirectoryEdit1.Directory + '\Screens.ino';
  setStatus('Generating output file ' + fileName);
  Log('Generating output file ' + filename);

  total := 0;

  AssignFile(txtFile, fileName);
  try
    Rewrite(txtFile);
    WriteLn(txtFile, '/**');
    WriteLn(txtFile, '* FormParser v.1.0.0 by Themelis Christos');
    WriteLn(txtFile, '* ');
    WriteLn(txtFile, '* Autogenerated file');
    WriteLn(txtFile, '* Screens: ' + IntToStr(screenIndex));
    WriteLn(txtFile, '*/');
    WriteLn(txtFile, '');
    WriteLn(txtFile, 'typedef struct Object {');
    WriteLn(txtFile, '  String name;');
    WriteLn(txtFile, '  bool enabled;');
    WriteLn(txtFile, '  int left;');
    WriteLn(txtFile, '  int top;');
    WriteLn(txtFile, '  int height;');
    WriteLn(txtFile, '  int width;');
    WriteLn(txtFile, '  String objType;');
    WriteLn(txtFile, '  String text;');
    WriteLn(txtFile, '  String onClick;');
    WriteLn(txtFile, '  int fontHeight;');
    WriteLn(txtFile, '  byte pictureData[];');
    WriteLn(txtFile, '}myObject;');
    WriteLn(txtFile, '');
    WriteLn(txtFile, 'typedef struct myScreen {');
    WriteLn(txtFile, '  int count;');
    WriteLn(txtFile, '  myObject myObjects[' + IntToStr(maxObjectsInScreen-1) + '];');
    WriteLn(txtFile, '};');
    WriteLn(txtFile, '');
    WriteLn(txtFile, 'myScreen myScreens[' + IntToStr(screenIndex) + '];');
    WriteLn(txtFile, '');
    WriteLn(txtFile, 'void initScreens() {');
    for i := 0 to screenIndex - 1 do
      begin
        WriteLn(txtFile, '');
        WriteLn(txtFile, '/**');
        WriteLn(txtFile, '* Screen: ' + myScreens[i].myObjects[0].name);
        WriteLn(txtFile, '*/');
        for j := 1 to myScreens[i].count - 1 do
          begin
            WriteLn(txtFile, '');
            WriteLn(txtFile, '// Object: ' + myScreens[i].myObjects[j].name);
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].name = "' + myScreens[i].myObjects[j].name + '";');
            if myScreens[i].myObjects[j].enabled = true then
               WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].enabled = true;')
            else
               WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].enabled = false;');

            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].left = ' + IntToStr(myScreens[i].myObjects[j].left) + ';');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].top = ' + IntToStr(myScreens[i].myObjects[j].top) + ';');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].height = ' + IntToStr(myScreens[i].myObjects[j].height) + ';');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].width = ' + IntToStr(myScreens[i].myObjects[j].width) + ';');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].objType = "' + myScreens[i].myObjects[j].objType + '";');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].text = "' + myScreens[i].myObjects[j].text + '";');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].onClick = "' + myScreens[i].myObjects[j].OnClick + '";');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].fontHeight = ' + IntToStr(myScreens[i].myObjects[j].FontHeight) + ';');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].fontColor = ' + IntToStr(myScreens[i].myObjects[j].FontColor) + ';');
            WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].backColor = ' + IntToStr(myScreens[i].myObjects[j].BackColor) + ';');
            len := length(myScreens[i].myObjects[j].PictureData);
            if len > 0 then
               begin
                 WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].pictureData[' + IntToStr((len div 2)+1) + '] = {');
                 x := 1;
                 s := '';
                 numbersPerLine := 0;
                 while x < len do
                   begin
                     s := s + '0x' + Copy(myScreens[i].myObjects[j].PictureData, x, 2) + ', ';
                     Inc(numbersPerLine);
                     if numbersPerLine > 9 then
                        begin
                          if (x+2) >= len then
                             WriteLn(txtFile, '  ' + Copy(s, 1, length(s)-2))
                          else
                             WriteLn(txtFile, '  ' + s);
                          s := '';
                          numbersPerLine := 0;
                        end;
                     x := x + 2;
                   end;
                 if length(s) > 0 then
                    WriteLn(txtFile, '  ' + Copy(s, 1, length(s)-2));
                 WriteLn(txtFile, '};');
               end;
            //else
            //   WriteLn(txtFile, ' myScreens[' + IntToStr(i) + '].myObjects[' + IntToStr(j-1) + '].pictureData[] = { 0xFF, 0xFF };');
            Inc(total);
          end;
      end;
    WriteLn(txtFile, '}');
    WriteLn(txtFile, '');
    WriteLn(txtFile, '/**');
    WriteLn(txtFile, '* Total objects: ' + IntToStr(total));
    WriteLn(txtFile, '* Autogenerated file ends.');
    WriteLn(txtFile, '*/');
  finally
    CloseFile(txtFile);
  end;

  AssignFile(TxtFile, DirectoryEdit1.Directory + '\' + FormatDateTime('yyyymmdd-hhmmss', Now) + '.log');
  try
    Rewrite(txtFile);
    WriteLn(txtFile, Memo1.Lines.GetText);
  finally
    CloseFile(txtFile);
  end;


  DirectoryEdit1.Enabled := true;
  btnSearch.Enabled := true;
  btnParse.Enabled := true;

  Log('Operation completed');
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
                if (SR.Name <> '.') and (SR.Name <> '..') and searchRecursively.Checked then
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

