unit notificationView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TfrmNotificationView }

  TfrmNotificationView = class(TForm)
    BatteryPercentage: TPanel;
    Clock: TPanel;
    NotificationAppName: TPanel;
    Bottom_Bar: TPanel;
    NotificationAdditionalInfo: TPanel;
    NotificationText: TMemo;
    StatusIcons: TPanel;
    NotificationActionDelete: TImage;
    NotificationTime: TPanel;
    NotificationBar: TPanel;
    NotificationAppIcon: TImage;
    StatusBar: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NotificationActionDeleteClick(Sender: TObject);
    procedure NotificationAppNameClick(Sender: TObject);
    procedure Notification_TextChange(Sender: TObject);
  private

  public

  end;

var
  frmNotificationView: TfrmNotificationView;

implementation

{$R *.lfm}

{ TfrmNotificationView }

procedure TfrmNotificationView.Button2Click(Sender: TObject);
begin

end;

procedure TfrmNotificationView.FormCreate(Sender: TObject);
begin

end;

procedure TfrmNotificationView.NotificationActionDeleteClick(Sender: TObject);
begin
  DeleteNotification(notificationID);
end;

procedure TfrmNotificationView.NotificationAppNameClick(Sender: TObject);
begin

end;

procedure TfrmNotificationView.Notification_TextChange(Sender: TObject);
begin

end;

end.

