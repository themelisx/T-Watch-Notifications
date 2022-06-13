package com.example.ble_notifications;

import java.io.Serializable;
import java.util.ArrayList;

public class NotificationList implements Serializable {
    int count = 0;
    ArrayList<NotificationBundle> nBundleList = new ArrayList<>();
}
