#include "BluetoothSerial.h"  // We will include the Serial Bluetooth header
#if !defined(CONFIG_BT_ENABLED) || !defined(CONFIG_BLUEDROID_ENABLED)
#error Bluetooth is not enabled! Please run `make menuconfig` to and enable it
#endif

#include <BLEDevice.h>
#include <BLEServer.h>
#include <BLEUtils.h>
#include <BLE2902.h>
#include <EEPROM.h>
#include <WiFi.h>
#include <Wire.h>

#include <ArduinoJson.h>

#include <string.h>
#include <string>
#include <cstdarg>

#include "features.h"
#include "config.h"
#include <soc/rtc.h>

#define DEBUG true  //set to true for debug output, false for no debug output
#define DEBUG_SERIAL if(DEBUG)Serial

#define vibrationPin 4

#define TFT_DEEPGREY 0x121414
//was 0x252920

#define DEFAULT_SCREEN_TIMEOUT 5000

TTGOClass *ttgo;

String Screen;

uint32_t targetTime = 0;  // for next 1 second display update

unsigned long lastTouch = 0;

uint8_t hh, mm, ss, mmonth, dday, weekDay;  // H, M, S, MM, DD, WD variables

uint16_t yyear;  // Year is 16 bit int

int16_t x, y;  // Touch coordinates

bool axpIrq;
bool keepAwake = false;

#ifdef BLE_NOTIFICATIONS
int notification_count = 0;

bool notification_new = false;

String newNotification;

char tmp_buffer[256];

inline std::string format(const char* fmt, ...){
    int size = 512;
    char* buffer = 0;
    buffer = new char[size];
    va_list vl;
    va_start(vl, fmt);
    int nsize = vsnprintf(buffer, size, fmt, vl);
    if(size<=nsize){ //fail delete buffer and try again
        delete[] buffer;
        buffer = 0;
        buffer = new char[nsize+1]; //+1 for /0
        nsize = vsnprintf(buffer, size, fmt, vl);
    }
    std::string ret(buffer);
    va_end(vl);
    delete[] buffer;
    return ret;
}

// Notifications ///////////////////////////////////////////////////////////////////

BLECharacteristic *pCharacteristic;
bool deviceConnected = false;
float txValue = 0;

void(* resetFunc) (void) = 0;

// See the following for generating UUIDs:
// https://www.uuidgenerator.net/

#define SERVICE_UUID           "6E400001-B5A3-F393-E0A9-E50E24DCCA9E" // UART service UUID
#define CHARACTERISTIC_UUID_RX "6E400002-B5A3-F393-E0A9-E50E24DCCA9E"
#define CHARACTERISTIC_UUID_TX "6E400003-B5A3-F393-E0A9-E50E24DCCA9E"

extern void parseCommand(String value);

class MyServerCallbacks: public BLEServerCallbacks {
    void onConnect(BLEServer* pServer) {
      DEBUG_SERIAL.println("Device connected");
      deviceConnected = true;

    };

    void onDisconnect(BLEServer* pServer) {
      DEBUG_SERIAL.println("Device disconnected");
      deviceConnected = false;

      delay(100);
      resetFunc();
    }
};

class MyCallbacks: public BLECharacteristicCallbacks {
    void onWrite(BLECharacteristic *pCharacteristic) {
      std::string rxValue = pCharacteristic->getValue();

      //DEBUG_SERIAL.println("Received value:");

      if (rxValue.length() > 0) {

        for (int i = 0; i < rxValue.length(); i++) {
          DEBUG_SERIAL.print(rxValue[i]);
        }
        DEBUG_SERIAL.println(" ");

        String value = rxValue.c_str();
        parseCommand(value);

      }
    }
};

#endif

// End-Notifications ///////////////////////////////////////////////////////////////////

void vibrate(int amount) {
  digitalWrite(vibrationPin, HIGH);
  delay(amount);
  digitalWrite(vibrationPin, LOW);
}

void setup() {
  ttgo = TTGOClass::getWatch();
  ttgo->begin();
  ttgo->tft->setTextFont(1);
  ttgo->tft->fillScreen(TFT_BLACK);
  ttgo->tft->setTextColor(TFT_WHITE, TFT_BLACK);  // Note: the new fonts do not draw the background colour
  //Initialize lvgl
  ttgo->lvgl_begin();

  //Check if the RTC clock matches, if not, use compile time
  ttgo->rtc->check();

  //Synchronize time to system time
  ttgo->rtc->syncToSystem();

  DEBUG_SERIAL.begin(115200);

  DEBUG_SERIAL.println("Starting setup...");

  pinMode(vibrationPin, OUTPUT);

  pinMode(AXP202_INT, INPUT_PULLUP);
  attachInterrupt(
    AXP202_INT, [] {
      axpIrq = true;
    },
    FALLING);

  ttgo->power->enableIRQ(AXP202_PEK_SHORTPRESS_IRQ | AXP202_VBUS_REMOVED_IRQ | AXP202_VBUS_CONNECT_IRQ | AXP202_CHARGING_IRQ, true);
  ttgo->power->clearIRQ();

  RTC_Date tnow = ttgo->rtc->getDateTime();
  hh = tnow.hour;
  mm = tnow.minute;
  dday = tnow.day;
  mmonth = tnow.month;
  yyear = tnow.year;

  #ifdef BLE_NOTIFICATIONS
  DEBUG_SERIAL.println("Notifications enabled");
  // Create the BLE Device
  BLEDevice::init("BLE-ESP32");  // Give it a name

  // Create the BLE Server
  BLEServer *pServer = BLEDevice::createServer();
  pServer->setCallbacks(new MyServerCallbacks());

  // Create the BLE Service
  BLEService *pService = pServer->createService(SERVICE_UUID);

  // Create a BLE Characteristic
  pCharacteristic = pService->createCharacteristic(
    CHARACTERISTIC_UUID_TX,
    BLECharacteristic::PROPERTY_NOTIFY);

  pCharacteristic->addDescriptor(new BLE2902());

  BLECharacteristic *pCharacteristic = pService->createCharacteristic(
    CHARACTERISTIC_UUID_RX,
    BLECharacteristic::PROPERTY_WRITE);

  pCharacteristic->setCallbacks(new MyCallbacks());

  // Start the service
  pService->start();

  // Start advertising
  pServer->getAdvertising()->start();
  DEBUG_SERIAL.println("Waiting connection...");
#endif
}

///////////////////////////////////////////////////////////////////// SETUP END - LOOP START

void loop() {
  
  if (axpIrq) {
    axpIrq = false;
    ttgo->power->readIRQ();
    if (ttgo->power->isPEKShortPressIRQ()) {
      DEBUG_SERIAL.println("button pressed");
      low_energy();
    }
    if (ttgo->power->isVbusPlugInIRQ()) {
      if (!ttgo->bl->isOn()) {
        low_energy();
      } else {
        displayTime(true);
      }
    }
    ttgo->power->clearIRQ();
  }

   if (ttgo->bl->isOn()) {
     
     if (ttgo->getTouch(x, y)) {

#ifdef BLE_NOTIFICATIONS
       if (y > 190 && y < 210) {
        viewNewNotification();
      }
#endif
     }
   }
   
   delay(50);
}

///////////////////////////////////////////////////////////////////// LOOP END