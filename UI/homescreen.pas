unit HomeScreen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmHomeScreen }

  TfrmHomeScreen = class(TForm)
    BatteryPercentage: TPanel;
    StatusIcons: TPanel;
    Label1: TLabel;
    Panel1: TPanel;
    Status_Bar: TPanel;
    Widget: TPanel;
    procedure TimeViewHomeClick(Sender: TObject);
  private

  public

  end;

var
  frmHomeScreen: TfrmHomeScreen;

implementation

{$R *.lfm}

{ TfrmHomeScreen }

procedure TfrmHomeScreen.TimeViewHomeClick(Sender: TObject);
begin

end;

end.

