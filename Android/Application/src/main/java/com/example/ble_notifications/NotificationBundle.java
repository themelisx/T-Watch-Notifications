package com.example.ble_notifications;

import java.io.Serializable;

class NotificationBundle implements Serializable {

    int id;
    String pName;       // Package name
    String appName;     // Application name (label)
    String category;
    String title;
    String text;
    String subText;
}
