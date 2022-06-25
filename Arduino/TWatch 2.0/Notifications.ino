
#ifdef BLE_NOTIFICATIONS
///////////////////////////////////////////////////////////////////// BLUETOOTH


void getNotificationList() {
    DEBUG_SERIAL.println("Requesting notification list...");
    int notificationType = 0; //All notifications
    sprintf(tmp_buffer, "GET_NOTIF_LIST=%d", notificationType);
    pCharacteristic->setValue(tmp_buffer);
    pCharacteristic->notify();
}

void createNotificationList(String value) {
  DEBUG_SERIAL.println("createNotificationList:");
  DEBUG_SERIAL.println(value);
}

void parseCommand(String value) {

    if (value.startsWith("ECHO=")) {
      value.replace("ECHO=", "");
      DEBUG_SERIAL.println(value);
    } else if (value.startsWith("NOTIFICATION_LIST")) {
      value.replace("NOTIFICATION_LIST=", "");
      createNotificationList(value);      
    } else if (value.startsWith("NEW_NOTIFICATION=")) {
      value.replace("NEW_NOTIFICATION=", "");
      alertNewNotification(value);      
    } else if (value.startsWith("GET_LIST")) {
      getNotificationList();
    } else {
      DEBUG_SERIAL.print("Unknown command: ");
      DEBUG_SERIAL.println(value);
    }
}

void alertNewNotification(String value) {
    DEBUG_SERIAL.print("New notification: ");
    DEBUG_SERIAL.println(value);
    // Do something to alert user for the new notification:
    newNotification = value;
    notification_count++;
    vibrate(50);
    delay(50);
    vibrate(50);
    notification_new = true;
    if(!ttgo->bl->isOn()){
      low_energy();
    }
}

///////////////////////////////////////////////////////////////////// BLUETOOTH END

///////////////////////////////////////////////////////////////////// NOTIFICATION FUNCTIONS

void viewNewNotification(){
  
  Screen = "notification_view";
  ttgo->tft->setTextSize(2);
  ttgo->tft->fillScreen(TFT_BLACK);
  ttgo->tft->setCursor(5,5);
  ttgo->tft->println(newNotification);
}
#endif