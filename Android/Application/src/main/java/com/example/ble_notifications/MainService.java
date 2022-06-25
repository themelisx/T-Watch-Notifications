package com.example.ble_notifications;

import static com.example.ble_notifications.BLE_Service.UUID_READ_FROM_ESP;
import static com.example.ble_notifications.BLE_Service.UUID_WRITE_TO_ESP;

import android.app.Service;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.Nullable;

import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MainService extends Service {

    // Constants
    private final static String TAG = MainService.class.getSimpleName();
    public static final String DEVICE_ADDRESS = "device_address";

    public final static String NOTIFICATION_ACTION = "com.example.NOTIFICATION_LISTENER_EXAMPLE";
    public final static String GET_NOTIFICATION_INTENT = "com.example.NOTIFICATION_LISTENER_SERVICE_EXAMPLE";

    private NotificationReceiver nReceiver = new NotificationReceiver();
    private String mDeviceAddress;
    private BLE_Service mBLEService;
    private ArrayList<ArrayList<BluetoothGattCharacteristic>> mGattCharacteristics = new ArrayList<ArrayList<BluetoothGattCharacteristic>>();
    public static boolean mConnected = false;
    private BluetoothGattCharacteristic mNotifyCharacteristic;
    private BluetoothGattCharacteristic mWriteCharacteristic;

    private final String LIST_NAME = "NAME";
    private final String LIST_UUID = "UUID";

    public MainService() {
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent != null) {
            mDeviceAddress = intent.getStringExtra(DEVICE_ADDRESS);
        } else {
            mDeviceAddress = "";
        }

        registerReceiver(nReceiver, makeNLServiceIntentFilter());
        registerReceiver(mGattUpdateReceiver, makeGattUpdateIntentFilter());

        Intent gattServiceIntent = new Intent(this, BLE_Service.class);
        bindService(gattServiceIntent, mServiceConnection, BIND_AUTO_CREATE);

        /*
        if (mBLEService != null) {
            final boolean result = mBLEService.connect(mDeviceAddress);
            Log.d(TAG, "Connect request result=" + result);
        }*/
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        unbindService(mServiceConnection);
        unregisterReceiver(mGattUpdateReceiver);
        unregisterReceiver(nReceiver);

        super.onDestroy();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    public String getApplicationName(String packageName) {
        Log.e(TAG, "get: " + packageName);
        try {
            PackageManager pm = getPackageManager();
            //ApplicationInfo ai = pm.getApplicationInfo(packageName, 0);
            return (String) pm.getApplicationLabel(pm.getApplicationInfo(packageName, PackageManager.GET_META_DATA));
        } catch (PackageManager.NameNotFoundException e) {
            return "Unknown app";
        }
    }

    class NotificationReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {

            if (intent.hasExtra("type")) {
                Bundle extras = intent.getExtras();
                String type = extras.getString("type");
                NotificationBundle notificationBundle = new NotificationBundle();

                if (type.equalsIgnoreCase("new_notification")) {

                    notificationBundle.id = extras.getInt("id", 0);
                    notificationBundle.pName = extras.getString("package", "");
                    notificationBundle.appName = getApplicationName(notificationBundle.pName);
                    notificationBundle.category = extras.getString("category", "");
                    notificationBundle.title = extras.getString("title", "");
                    notificationBundle.text = extras.getString("text", "");
                    if (notificationBundle.category.equals("email")) {
                        notificationBundle.subText = extras.getString("sub_text", "");
                    }
                    sendData("NEW_NOTIFICATION=" + new Gson().toJson(notificationBundle));

                } else if (type.equalsIgnoreCase("list_notification")) {

                    sendData("NOTIFICATION_LIST=" + extras.getString("data", ""));

                } else {
                    Log.e(TAG, "Unknown type:" + type);
                }
            }
        }
    }

    private final ServiceConnection mServiceConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName componentName, IBinder service) {
            mBLEService = ((BLE_Service.LocalBinder) service).getService();
            if (!mBLEService.initialize()) {
                Log.e(TAG, "Unable to initialize Bluetooth");
                mBLEService.disconnect();
            }
            // Automatically connects to the device upon successful start-up initialization.
            mBLEService.connect(mDeviceAddress);
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            mBLEService = null;
        }
    };

    // Handles various events fired by the Service.
    // ACTION_GATT_CONNECTED: connected to a GATT server.
    // ACTION_GATT_DISCONNECTED: disconnected from a GATT server.
    // ACTION_GATT_SERVICES_DISCOVERED: discovered GATT services.
    // ACTION_DATA_AVAILABLE: received data from the device.  This can be a result of read
    //                        or notification operations.
    private final BroadcastReceiver mGattUpdateReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {

            final String action = intent.getAction();
            if (BLE_Service.ACTION_GATT_CONNECTED.equals(action)) {

                mConnected = true;
                Log.i(TAG, getString(R.string.connected));

                // TODO: Update UI
                // updateConnectionState(R.string.connected);

            } else if (BLE_Service.ACTION_GATT_DISCONNECTED.equals(action)) {
                mConnected = false;

                Log.i(TAG, getString(R.string.disconnected));

                // TODO: Update UI
                // updateConnectionState(R.string.disconnected);
            } else if (BLE_Service.ACTION_GATT_SERVICES_DISCOVERED.equals(action)) {
                // Show all the supported services and characteristics on the user interface.
                displayGattServices(mBLEService.getSupportedGattServices());
            } else if (BLE_Service.ACTION_DATA_AVAILABLE.equals(action)) {

                String s = intent.getStringExtra(BLE_Service.EXTRA_DATA);
                Log.i(TAG, "ESP says: " + s);

                if (s.startsWith("ESP32=")) {

                    s = s.replace("ESP32=", "");
                    Log.d(TAG, "Command: ESP32");
                    Log.d(TAG, "Value: " + s);

                } else if (s.startsWith("GET_NOTIF_LIST=")) {

                    s = s.replace("GET_NOTIF_LIST=", "");
                    Log.d(TAG, "Command: Get Notification List");
                    Log.d(TAG, "Value: " + s);

                    //get the current notifications by broadcasting an intent
                    Intent i = new Intent(GET_NOTIFICATION_INTENT);
                    i.putExtra("command", "list");
                    sendBroadcast(i);

                } else { //GET_NOTIFICATION_LIST
                    Log.e(TAG, "Unknown command");
                }
            } /*else {
                Log.e(TAG, "Action=" + action);
            }*/
        }
    };

    public void sendData(String data) {

        if (mConnected) {
            if (mWriteCharacteristic != null) {
                byte[] strBytes = data.getBytes();
                mWriteCharacteristic.setValue(strBytes);
                mBLEService.writeCharacteristic(mWriteCharacteristic);

            } else {
                Log.e(TAG, "mWriteCharacteristic is null");
                doDisconnect(null);
            }
        } else {
            Toast.makeText(this, getString(R.string.device_not_connected), Toast.LENGTH_SHORT).show();
        }
    }

    private static IntentFilter makeNLServiceIntentFilter() {
        final IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(NOTIFICATION_ACTION);
        return intentFilter;
    }

    private static IntentFilter makeGattUpdateIntentFilter() {
        final IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(BLE_Service.ACTION_GATT_CONNECTED);
        intentFilter.addAction(BLE_Service.ACTION_GATT_DISCONNECTED);
        intentFilter.addAction(BLE_Service.ACTION_GATT_SERVICES_DISCOVERED);
        intentFilter.addAction(BLE_Service.ACTION_DATA_AVAILABLE);
        return intentFilter;
    }

    public void doDisconnect(View view) {
        if (mBLEService != null) {
            mBLEService.disconnect();
        }
    }

    // Demonstrates how to iterate through the supported GATT Services/Characteristics.
    // In this sample, we populate the data structure that is bound to the ExpandableListView
    // on the UI.
    private void displayGattServices(List<BluetoothGattService> gattServices) {
        if (gattServices == null) return;
        String uuid = null;
        String unknownServiceString = getResources().getString(R.string.unknown_service);
        String unknownCharaString = getResources().getString(R.string.unknown_characteristic);
        ArrayList<HashMap<String, String>> gattServiceData = new ArrayList<HashMap<String, String>>();
        ArrayList<ArrayList<HashMap<String, String>>> gattCharacteristicData
                = new ArrayList<ArrayList<HashMap<String, String>>>();
        mGattCharacteristics = new ArrayList<ArrayList<BluetoothGattCharacteristic>>();

        // Loops through available GATT Services.
        for (BluetoothGattService gattService : gattServices) {
            HashMap<String, String> currentServiceData = new HashMap<String, String>();
            uuid = gattService.getUuid().toString();
            currentServiceData.put(
                    LIST_NAME, SampleGattAttributes.lookup(uuid, unknownServiceString));
            currentServiceData.put(LIST_UUID, uuid);
            gattServiceData.add(currentServiceData);

            ArrayList<HashMap<String, String>> gattCharacteristicGroupData =
                    new ArrayList<HashMap<String, String>>();
            List<BluetoothGattCharacteristic> gattCharacteristics =
                    gattService.getCharacteristics();
            ArrayList<BluetoothGattCharacteristic> charas =
                    new ArrayList<BluetoothGattCharacteristic>();

            // Loops through available Characteristics.
            for (BluetoothGattCharacteristic gattCharacteristic : gattCharacteristics) {
                charas.add(gattCharacteristic);
                HashMap<String, String> currentCharaData = new HashMap<String, String>();
                uuid = gattCharacteristic.getUuid().toString();
                currentCharaData.put(
                        LIST_NAME, SampleGattAttributes.lookup(uuid, unknownCharaString));
                currentCharaData.put(LIST_UUID, uuid);
                gattCharacteristicGroupData.add(currentCharaData);

                if (gattCharacteristic.getUuid().equals(UUID_READ_FROM_ESP)) {
                    mNotifyCharacteristic = gattCharacteristic;
                    mBLEService.setCharacteristicNotification(gattCharacteristic, true);
                }

                if (gattCharacteristic.getUuid().equals(UUID_WRITE_TO_ESP)) {
                    mWriteCharacteristic = gattCharacteristic;
                }
            }
            mGattCharacteristics.add(charas);
            gattCharacteristicData.add(gattCharacteristicGroupData);
        }
    }
}