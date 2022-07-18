unit alarm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAlarm }

  TfrmAlarm = class(TForm)
    BatteryPercentage: TPanel;
    btnCancel: TButton;
    btnSnooze: TButton;
    NotificationIcons: TPanel;
    Label3: TLabel;
    AlarmTitle: TPanel;
    AlarmClock: TPanel;
    Clock: TPanel;
    StatusBar: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSnoozeClick(Sender: TObject);
  private

  public

  end;

var
  frmAlarm: TfrmAlarm;

implementation

{$R *.lfm}

{ TfrmAlarm }

procedure TfrmAlarm.btnCancelClick(Sender: TObject);
begin
  CancelAlarm();
end;

procedure TfrmAlarm.btnSnoozeClick(Sender: TObject);
begin
  SnoozeAlarm();
end;

{ TfrmAlarm }

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

end.

