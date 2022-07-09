#ifdef BLE_NOTIFICATIONS
///////////////////////////////////////////////////////////////////// BLUETOOTH

void requestIcon(String PKGNAME) {
  sprintf(tmp_buffer, "GET_PKG_ICON=%s", PKGNAME);
  pCharacteristic->setValue(tmp_buffer);
  pCharacteristic->notify();
}

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

  } else if (value.startsWith("ICON=")) {
    value.replace("ICON=", "");

    int inputStringLength = value.length();  //Get length of input
    char *inputString = (char *)malloc(inputStringLength + 1);
    value.getBytes((unsigned char *)inputString, inputStringLength);

    int decodedLength = BASE64::decodeLength(inputString);
    uint8_t *base64Result = (uint8_t *)malloc(inputStringLength);

    BASE64::decode(inputString, base64Result);

    drawArrayJpeg(base64Result, sizeof(base64Result), 0, 0);  //last two are coordinates to draw image

    free(base64Result);
    free(inputString);

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
  String pName = String((const char *)myObject["pName"]);  //package name

  requestIcon(pName);

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

  u8f.setForegroundColor(TFT_WHITE);  // apply color
  ttgo->tft->fillScreen(TFT_BLACK);

  u8f.setFont(u8g2_font_unifont_t_greek);  //greek font (standard size)
  //u8f.setTextSize(2);
  u8f.setCursor(5, 40);

  u8f.print("AppName: ");
  u8f.println(appName);
  u8f.print("Text: ");
  u8f.println(text);
  u8f.print("Title: ");
  u8f.println(title);
  u8f.print("SubText: ");
  u8f.println(subText);
}


//####################################################################################################
// Draw a JPEG on the TFT pulled from a program memory array
//####################################################################################################
void drawArrayJpeg(const uint8_t arrayname[], uint32_t array_size, int x, int y) {

  JpegDec.decodeArray(arrayname, array_size);

  jpegInfo();  // Print information from the JPEG file (could comment this line out)

  renderJPEG(x, y);

  Serial.println("#########################");
}

//####################################################################################################
// Draw a JPEG on the TFT, images will be cropped on the right/bottom sides if they do not fit
//####################################################################################################
// This function assumes xpos,ypos is a valid screen coordinate. For convenience images that do not
// fit totally on the screen are cropped to the nearest MCU size and may leave right/bottom borders.
/*void renderJPEG(int xpos, int ypos) {

  // retrieve infomration about the image
  uint16_t *pImg;
  uint16_t mcu_w = JpegDec.MCUWidth;
  uint16_t mcu_h = JpegDec.MCUHeight;
  uint32_t max_x = JpegDec.width;
  uint32_t max_y = JpegDec.height;

  // Jpeg images are draw as a set of image block (tiles) called Minimum Coding Units (MCUs)
  // Typically these MCUs are 16x16 pixel blocks
  // Determine the width and height of the right and bottom edge image blocks
  uint32_t min_w = min(mcu_w, max_x % mcu_w);
  uint32_t min_h = min(mcu_h, max_y % mcu_h);

  // save the current image block size
  uint32_t win_w = mcu_w;
  uint32_t win_h = mcu_h;

  // read each MCU block until there are no more
  while (JpegDec.read()) {  // While there is more data in the file
    // save a pointer to the image block
    pImg = JpegDec.pImage;
    // calculate where the image block should be drawn on the screen
    int mcu_x = JpegDec.MCUx * mcu_w + xpos;  // Calculate coordinates of top left corner of current MCU
    int mcu_y = JpegDec.MCUy * mcu_h + ypos;
    // check if the image block size needs to be changed for the right and bottom edges
    if (mcu_x + mcu_w <= max_x) win_w = mcu_w;
    else
      win_w = min_w;
    if (mcu_y + mcu_h <= max_y) win_h = mcu_h;
    else
      win_h = min_h;

    // calculate how many pixels must be drawn
    uint32_t mcu_pixels = win_w * win_h;

    // Write all MCU pixels to the TFT window
    while (mcu_pixels--) {
      // Push each pixel to the TFT MCU area
      uint8_t col_h = (*pImg) >> 8;         // High byte
      uint8_t col_l = (*pImg) & 0xFF;       // Low byte
      pImg++;                               // Increment pointer
      myGLCD.LCD_Write_DATA(col_h, col_l);  // Sent pixel colour to window
    }
  }
}*/

//====================================================================================
//   Print information about the decoded Jpeg image
//====================================================================================

void jpegInfo() {
  Serial.println(F("==============="));
  Serial.println(F("JPEG image info"));
  Serial.println(F("==============="));
  Serial.print(F("Width      :"));
  Serial.println(JpegDec.width);
  Serial.print(F("Height     :"));
  Serial.println(JpegDec.height);
  Serial.print(F("Components :"));
  Serial.println(JpegDec.comps);
  Serial.print(F("MCU / row  :"));
  Serial.println(JpegDec.MCUSPerRow);
  Serial.print(F("MCU / col  :"));
  Serial.println(JpegDec.MCUSPerCol);
  Serial.print(F("Scan type  :"));
  Serial.println(JpegDec.scanType);
  Serial.print(F("MCU width  :"));
  Serial.println(JpegDec.MCUWidth);
  Serial.print(F("MCU height :"));
  Serial.println(JpegDec.MCUHeight);
  Serial.println(F("==============="));
}
#endif