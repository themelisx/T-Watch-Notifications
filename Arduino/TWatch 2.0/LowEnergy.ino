bool hasLowEnergy;

void low_energy() {

  if (!keepAwake) {
    if (ttgo->bl->isOn()) {
      DEBUG_SERIAL.println("backlight on, turning off");
      ttgo->closeBL();
      ttgo->stopLvglTick();
      ttgo->displaySleep();

      hasLowEnergy = true;
      DEBUG_SERIAL.println("screen off");

    } else {
      hasLowEnergy = false;
      DEBUG_SERIAL.println("Waking up");
      ttgo->displayWakeup();
      ttgo->rtc->syncToSystem();
      lv_disp_trig_activity(NULL);
      ttgo->openBL();
      
      RTC_Date tnow = ttgo->rtc->getDateTime();
      hh = tnow.hour;
      mm = tnow.minute;

      lastTouch = millis();
      DEBUG_SERIAL.println(lastTouch);
      DEBUG_SERIAL.println("wakeup");

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