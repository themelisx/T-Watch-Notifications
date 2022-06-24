bool hasLowEnergy;

void low_energy() {

  if (!keepAwake) {
    if (ttgo->bl->isOn()) {
      Serial.println("backlight on, turning off");
      ttgo->closeBL();
      ttgo->stopLvglTick();
      ttgo->displaySleep();

      hasLowEnergy = true;
      Serial.println("screen off");

    } else {
      hasLowEnergy = false;
      Serial.println("Waking up");
      ttgo->displayWakeup();
      ttgo->rtc->syncToSystem();
      lv_disp_trig_activity(NULL);
      ttgo->openBL();
      
      RTC_Date tnow = ttgo->rtc->getDateTime();
      hh = tnow.hour;
      mm = tnow.minute;

      lastTouch = millis();
      Serial.println(lastTouch);
      Serial.println("wakeup");

      #ifndef BLE_Notifications
        displayTime(true);
      #else
        if(notif_new){
          notif_new = false;
          viewNewNotification();
        } else {
          displayTime(true);
        }
      #endif
    }
  }
}