#include <time.h>

byte xcolon = 0;  // location of the colon

void displayTime(boolean fullUpdate) {

  Screen = "home";
  byte xpos = 60;  // Stating position for the time display
  byte ypos = 90;

  String dayOfWeek;

  // Get the current data
  RTC_Date tnow = ttgo->rtc->getDateTime();

  hh = tnow.hour;  //Get time data
  mm = tnow.minute;
  ss = tnow.second;
  dday = tnow.day;
  mmonth = tnow.month;
  yyear = tnow.year;

  if (fullUpdate) {
    lastTouch = millis();
    Serial.println(lastTouch);
    Serial.println("display time");

    ////Get date of week in words
    weekDay = ttgo->rtc->getDayOfWeek(dday, mmonth, yyear);
    switch (weekDay) {
      case 0:
        dayOfWeek = "Sun";
        break;
      case 1:
        dayOfWeek = "Mon";
        break;
      case 2:
        dayOfWeek = "Tue";
        break;
      case 3:
        dayOfWeek = "Wed";
        break;
      case 4:
        dayOfWeek = "Thu";
        break;
      case 5:
        dayOfWeek = "Fri";
        break;
      case 6:
        dayOfWeek = "Sat";
        break;
    }

    ////Print time
    ttgo->tft->fillScreen(TFT_BLACK);  //Empty screen
    ttgo->tft->setTextColor(0xFFFF);

    if (hh < 10) xpos += ttgo->tft->drawChar('0', xpos, ypos, 6);  //Add a zero if hours < 10
    xpos += ttgo->tft->drawNumber(hh, xpos, ypos, 6);
    xcolon = xpos + 3;
    xpos += ttgo->tft->drawChar(':', xcolon, ypos, 6);                 //Draw a colon
    if (mm < 10) xpos += ttgo->tft->drawChar('0', xpos + 4, ypos, 6);  //Add a zero if minutes < 0
    ttgo->tft->drawNumber(mm, xpos + 4, ypos, 6);


    ////Get date
    char date[15];
    memset(date, 0, 15);
    sprintf(date, "%s %d/%d/%d", dayOfWeek, dday, mmonth, yyear);

    ////Print date
    int xPos = ttgo->tft->textWidth(date, 4);
    ttgo->tft->setTextSize(1);
    ttgo->tft->setCursor(70, 60);
    ttgo->tft->drawString(date, 120 - (xPos / 2), 50, 4);

    ////Get battery
    int p = ttgo->power->getBattPercentage();
    if (p > 100) {
      p = 100;
    }
    char percentage[5];
    memset(percentage, 0, 5);
    sprintf(percentage, "%d%%", p);

    ////Print battery
    xPos = 240 - ttgo->tft->textWidth(percentage, 4);

    ttgo->tft->fillRect(166, 0, 74, 30, TFT_BLACK);
    ttgo->tft->drawString(percentage, xPos, 5, 4);

#ifdef BLE_NOTIFICATIONS
    ////Draw notification widget
    sprintf(tmp_buffer, "Notifications: %d", notification_count);

    ttgo->tft->drawString(tmp_buffer, 5, 205, 4);
#endif
  }
}