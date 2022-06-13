#include <BLEDevice.h>
#include <BLEServer.h>
#include <BLEUtils.h>
#include <BLE2902.h>
#include <EEPROM.h>
#include <WiFi.h>
#include <Wire.h>

char tmp_buffer[256];
BLECharacteristic *pCharacteristic;
bool deviceConnected = false;
float txValue = 0;

// See the following for generating UUIDs:
// https://www.uuidgenerator.net/

#define SERVICE_UUID           "6E400001-B5A3-F393-E0A9-E50E24DCCA9E" // UART service UUID
#define CHARACTERISTIC_UUID_RX "6E400002-B5A3-F393-E0A9-E50E24DCCA9E"
#define CHARACTERISTIC_UUID_TX "6E400003-B5A3-F393-E0A9-E50E24DCCA9E"

void(* resetFunc) (void) = 0;

void getNotificationList() {
    Serial.println("Requesting notification list...");
    int notificationType = 0; //All notifications
    sprintf(tmp_buffer, "GET_NOTIF_LIST=%d", notificationType);
    pCharacteristic->setValue(tmp_buffer);
    pCharacteristic->notify();
}

void createNotificationList(String value) {
  Serial.println("createNotificationList:");
  Serial.println(value);
}

void parseCommand(String value) {

    if (value.startsWith("ECHO=")) {
      value.replace("ECHO=", "");
      Serial.println(value);
    } else if (value.startsWith("NOTIFICATION_LIST")) {
      value.replace("NOTIFICATION_LIST=", "");
      createNotificationList(value);      
    } else if (value.startsWith("NEW_NOTIFICATION=")) {
      value.replace("NEW_NOTIFICATION=", "");
      alertNewNotification(value);      
    } else if (value.startsWith("GET_LIST")) {
      getNotificationList();
    } else {
      Serial.print("Unknown command: ");
      Serial.println(value);
    }
}

class MyServerCallbacks: public BLEServerCallbacks {
    void onConnect(BLEServer* pServer) {
      Serial.println("Device connected");
      deviceConnected = true;

    };

    void onDisconnect(BLEServer* pServer) {
      Serial.println("Device disconnected");
      deviceConnected = false;

      delay(100);
      resetFunc();
    }
};

class MyCallbacks: public BLECharacteristicCallbacks {
    void onWrite(BLECharacteristic *pCharacteristic) {
      std::string rxValue = pCharacteristic->getValue();

      //Serial.println("Received value:");

      if (rxValue.length() > 0) {

        for (int i = 0; i < rxValue.length(); i++) {
          Serial.print(rxValue[i]);
        }
        Serial.println(" ");

        String value = rxValue.c_str();
        parseCommand(value);

      }
    }
};

void alertNewNotification(String value) {
    Serial.print("New notification: ");
    Serial.println(value);
    // Do something to alert user for the new notification
}

void setup() {
  Serial.begin(115200);

  Serial.println("Starting setup...");

  // Create the BLE Device
  BLEDevice::init("BLE-ESP32"); // Give it a name

  // Create the BLE Server
  BLEServer *pServer = BLEDevice::createServer();
  pServer->setCallbacks(new MyServerCallbacks());

  // Create the BLE Service
  BLEService *pService = pServer->createService(SERVICE_UUID);

  // Create a BLE Characteristic
  pCharacteristic = pService->createCharacteristic(
                      CHARACTERISTIC_UUID_TX,
                      BLECharacteristic::PROPERTY_NOTIFY
                    );

  pCharacteristic->addDescriptor(new BLE2902());

  BLECharacteristic *pCharacteristic = pService->createCharacteristic(
                                         CHARACTERISTIC_UUID_RX,
                                         BLECharacteristic::PROPERTY_WRITE
                                       );

  pCharacteristic->setCallbacks(new MyCallbacks());

  // Start the service
  pService->start();

  // Start advertising
  pServer->getAdvertising()->start();
  Serial.println("Waiting connection...");

}

void loop() {

  delay(50);

}
