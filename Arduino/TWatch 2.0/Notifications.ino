#ifdef BLE_NOTIFICATIONS
///////////////////////////////////////////////////////////////////// BLUETOOTH


void getNotificationList() {
  DEBUG_SERIAL.println("Requesting notification list...");
  int notificationType = 0;  //All notifications
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
    //NEW_NOTIFICATION={"appName":"Messages","category":"msg","id":0,"pName":"com.google.android.apps.messaging","text":"MHNYMA AΠO KINHTO","title":"Μωρό μου"}
    //NEW_NOTIFICATION={"appName":"Gmail","category":"email","id":0,"pName":"com.google.android.gm","subText":"themelisx@gmail.com","text":"Hello","title":"Παναγιώτης Θ"}
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
  delay(100);
  vibrate(50);
  notification_new = true;
  if (!ttgo->bl->isOn()) {
    low_energy();
  }
  viewNewNotification();
}

///////////////////////////////////////////////////////////////////// BLUETOOTH END

///////////////////////////////////////////////////////////////////// NOTIFICATION FUNCTIONS

void viewNewNotification() {
  Screen = "notification_view";

  ttgo->tft->fillScreen(TFT_BLACK);  //Empty screen

  String jsonBuffer = newNotification;
  int x = jsonBuffer.length() + 1;
  char *buffer = (char *)malloc(x);
  jsonBuffer.toCharArray(buffer, x);
  x = x * 2;

  DynamicJsonDocument myObject(x);
  //Serial.print(jsonBuffer.length());
  DeserializationError err = deserializeJson(myObject, buffer);
  delay(200);
  if (err) {
    DEBUG_SERIAL.println("error with deserialization");
    DEBUG_SERIAL.println(err.c_str());
    ttgo->tft->drawString("Could not get", 10, 65, 2);
    ttgo->tft->drawString("data", 10, 85, 2);
    delay(2000);
    return;
  }
  //NEW_NOTIFICATION={"appName":"Messages","category":"msg","id":0,"pName":"com.google.android.apps.messaging","text":"MHNYMA AΠO KINHTO","title":"Μωρό μου"}

  String appName = String((const char *)myObject["appName"]);
  String category = String((const char *)myObject["category"]); 
  int id = (int)myObject["id"];
  String text;
  String title;
  String subText = "";

  if (category.equals("msg")) {
    text = String((const char *)myObject["text"]);
    title = String((const char *)myObject["title"]);
  } else if (category.equals("email")) {
    text = String((const char *)myObject["text"]);
    title = String((const char *)myObject["title"]);
    subText = String((const char *)myObject["subText"]);
  }

  DEBUG_SERIAL.println(appName);
  DEBUG_SERIAL.println(category);
  DEBUG_SERIAL.println(text);
  DEBUG_SERIAL.println(title);
  DEBUG_SERIAL.println(subText);

  ttgo->tft->setTextSize(2);
  ttgo->tft->fillScreen(TFT_BLACK);
  ttgo->tft->setCursor(5, 5);
  ttgo->tft->print("AppName: ");
  ttgo->tft->println(appName);
  ttgo->tft->print("Text: ");
  ttgo->tft->println(text);
  ttgo->tft->print("Title: ");
  ttgo->tft->println(title);
  ttgo->tft->print("SubText: ");
  ttgo->tft->println(subText);
}
#endif