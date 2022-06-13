/*
 * Copyright (C) 2013 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.example.ble_notifications;

import static com.example.ble_notifications.BluetoothLeService.UUID_READ_FROM_ESP;
import static com.example.ble_notifications.BluetoothLeService.UUID_WRITE_TO_ESP;

import android.app.ActionBar;
import android.app.Activity;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.provider.Settings;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.Toast;

import com.example.ble_notifications.R;
import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

/**
 * For a given BLE device, this Activity provides the user interface to connect, display data,
 * and display GATT services and characteristics supported by the device.  The Activity
 * communicates with {@code BluetoothLeService}, which in turn interacts with the
 * Bluetooth LE API.
 */
public class MainActivity extends Activity {

    private final static String TAG = MainActivity.class.getSimpleName();

    public static final String EXTRAS_DEVICE_NAME = "DEVICE_NAME";
    public static final String EXTRAS_DEVICE_ADDRESS = "DEVICE_ADDRESS";

    private String mDeviceName;
    private String mDeviceAddress;
    private BluetoothLeService mBluetoothLeService;
    private ArrayList<ArrayList<BluetoothGattCharacteristic>> mGattCharacteristics = new ArrayList<ArrayList<BluetoothGattCharacteristic>>();
    public static boolean mConnected = false;
    private BluetoothGattCharacteristic mNotifyCharacteristic;
    private BluetoothGattCharacteristic mWriteCharacteristic;

    private final String LIST_NAME = "NAME";
    private final String LIST_UUID = "UUID";

    private EditText textToSend;
    private Button btnSend;
    private Button getList;

    private ListAdapter mListAdapter;
    private ListView dataList;

    private NotificationReceiver nReceiver = new NotificationReceiver();;



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
                Log.v(TAG, "NotificationReceiver: onReceive");
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
            mBluetoothLeService = ((BluetoothLeService.LocalBinder) service).getService();
            if (!mBluetoothLeService.initialize()) {
                Log.e(TAG, "Unable to initialize Bluetooth");
                finish();
            }
            // Automatically connects to the device upon successful start-up initialization.
            mBluetoothLeService.connect(mDeviceAddress);
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            mBluetoothLeService = null;
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
            if (BluetoothLeService.ACTION_GATT_CONNECTED.equals(action)) {

                mConnected = true;

                Log.i(TAG, getString(R.string.connected));

                updateConnectionState(R.string.connected);
                invalidateOptionsMenu();

                btnSend.setEnabled(true);

            } else if (BluetoothLeService.ACTION_GATT_DISCONNECTED.equals(action)) {
                mConnected = false;

                Log.i(TAG, getString(R.string.disconnected));

                updateConnectionState(R.string.disconnected);
                invalidateOptionsMenu();

                btnSend.setEnabled(false);

                Toast.makeText(MainActivity.this, getString(R.string.disconnected), Toast.LENGTH_SHORT).show();
                final Handler handler = new Handler();
                handler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        final Intent i = new Intent(MainActivity.this, DeviceScanActivity.class);
                        i.putExtra("CONNECT_AUTO", false);
                        startActivity(i);
                        finish();
                        //doConnect(null);
                    }
                }, 1000);
            } else if (BluetoothLeService.ACTION_GATT_SERVICES_DISCOVERED.equals(action)) {
                // Show all the supported services and characteristics on the user interface.
                displayGattServices(mBluetoothLeService.getSupportedGattServices());
            } else if (BluetoothLeService.ACTION_DATA_AVAILABLE.equals(action)) {

                String s = intent.getStringExtra(BluetoothLeService.EXTRA_DATA);
                Log.i(TAG, "ESP says: " + s);

                if (s.startsWith("ESP32=")) {

                    s = s.replace("ESP32=", "");
                    Log.d(TAG, "Command: ESP32");
                    Log.d(TAG, "Value: " + s);
                    mListAdapter.addEspMessage(s);

                } else if (s.startsWith("GET_NOTIF_LIST=")) {

                    s = s.replace("GET_NOTIF_LIST=", "");
                    Log.d(TAG, "Command: Get Notification List");
                    Log.d(TAG, "Value: " + s);
                    mListAdapter.addEspMessage(s);

                    //get the current notifications by broadcasting an intent
                    Intent i = new Intent(NLService.GET_NOTIFICATION_INTENT);
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

    public void updateStatusText() {
        MainActivity.this.runOnUiThread(new Runnable() {
            @Override
            public void run() {

                Log.e(TAG, "Update UI");

            }
        });
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_main);

        //startActivity(new Intent("android.settings.ACTION_NOTIFICATION_LISTENER_SETTINGS"));

        textToSend = findViewById(R.id.text_to_send);
        btnSend = findViewById(R.id.btn_send);
        btnSend.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String data = textToSend.getText().toString();
                if (!data.isEmpty()) {
                    sendData("ECHO=" + data);
                }
            }
        });

        getList = findViewById(R.id.get_list);
        getList.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

                sendData("GET_LIST");
            }
        });

        dataList = findViewById(R.id.data_list);

        mListAdapter = new ListAdapter(this);
        dataList.setAdapter(mListAdapter);

        final Intent intent = getIntent();
        mDeviceName = intent.getStringExtra(EXTRAS_DEVICE_NAME);
        mDeviceAddress = intent.getStringExtra(EXTRAS_DEVICE_ADDRESS);

        // Sets up UI references.
        ActionBar actionBar = getActionBar();
        if (actionBar != null) {
            actionBar.setTitle(getString(R.string.app_name) + " - " + getString(R.string.init));
            actionBar.setDisplayHomeAsUpEnabled(true);
        }

        Intent gattServiceIntent = new Intent(this, BluetoothLeService.class);
        bindService(gattServiceIntent, mServiceConnection, BIND_AUTO_CREATE);

    }

    //https://stackoverflow.com/questions/22663359/redirect-to-notification-access-settings
    public boolean isNotificationServiceRunning() {
        ContentResolver contentResolver = getContentResolver();
        String enabledNotificationListeners =
                Settings.Secure.getString(contentResolver, "enabled_notification_listeners");
        String packageName = getPackageName();
        return enabledNotificationListeners != null && enabledNotificationListeners.contains(packageName);
    }

    public void gotoSettings(View view) {
        Intent intent = new Intent("android.settings.ACTION_NOTIFICATION_LISTENER_SETTINGS");
        startActivity(intent);
    }

    @Override
    protected void onResume() {
        super.onResume();

        registerReceiver(nReceiver, makeNLServiceIntentFilter());
        registerReceiver(mGattUpdateReceiver, makeGattUpdateIntentFilter());
        if (mBluetoothLeService != null) {
            final boolean result = mBluetoothLeService.connect(mDeviceAddress);
            Log.d(TAG, "Connect request result=" + result);
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        unregisterReceiver(mGattUpdateReceiver);
        unregisterReceiver(nReceiver);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        unbindService(mServiceConnection);
        mBluetoothLeService = null;
    }

    private void updateConnectionState(final int resourceId) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                ActionBar actionBar = getActionBar();
                if (actionBar != null) {
                    actionBar.setTitle(String.format(Locale.US, "%s - %s",
                            getString(R.string.app_name), getString(resourceId)));
                }
            }
        });
    }

    private void sendData(String data) {

        if (mConnected) {
            if (mWriteCharacteristic != null) {
                byte[] strBytes = data.getBytes();
                mWriteCharacteristic.setValue(strBytes);
                mBluetoothLeService.writeCharacteristic(mWriteCharacteristic);
                mListAdapter.addAndroidMessage(data);
                textToSend.setText("");
            } else {
                Log.e(TAG, "mWriteCharacteristic is null");
                doDisconnect(null);

                Toast.makeText(MainActivity.this, getString(R.string.disconnected), Toast.LENGTH_SHORT).show();
                final Handler handler = new Handler();
                handler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        final Intent i = new Intent(MainActivity.this, DeviceScanActivity.class);
                        i.putExtra("CONNECT_AUTO", false);
                        startActivity(i);
                        finish();
                    }
                }, 1000);
            }
        } else {
            Toast.makeText(this, getString(R.string.device_not_connected), Toast.LENGTH_SHORT).show();
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
                    //Log.e(TAG, "------------------ Found -------------------");
                    mNotifyCharacteristic = gattCharacteristic;
                    mBluetoothLeService.setCharacteristicNotification(gattCharacteristic, true);
                }

                if (gattCharacteristic.getUuid().equals(UUID_WRITE_TO_ESP)) {
                    mWriteCharacteristic = gattCharacteristic;
                    //Log.e(TAG, "------------------ Found -------------------");
                }
            }
            mGattCharacteristics.add(charas);
            gattCharacteristicData.add(gattCharacteristicGroupData);
        }
    }

    private static IntentFilter makeNLServiceIntentFilter() {
        final IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(NLService.NOTIFICATION_ACTION);
        return intentFilter;
    }

    private static IntentFilter makeGattUpdateIntentFilter() {
        final IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(BluetoothLeService.ACTION_GATT_CONNECTED);
        intentFilter.addAction(BluetoothLeService.ACTION_GATT_DISCONNECTED);
        intentFilter.addAction(BluetoothLeService.ACTION_GATT_SERVICES_DISCOVERED);
        intentFilter.addAction(BluetoothLeService.ACTION_DATA_AVAILABLE);
        return intentFilter;
    }

    public void doDisconnect(View view) {
        if (mBluetoothLeService != null) {
            mBluetoothLeService.disconnect();
        }
        //finish();
    }

    public void doConnect(View view) {
        if (mBluetoothLeService != null) {
            mBluetoothLeService.connect(mDeviceAddress);
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.gatt_services, menu);
        /*
        if (mConnected) {
            menu.findItem(R.id.menu_connect).setVisible(false);
            menu.findItem(R.id.menu_disconnect).setVisible(true);
        } else {
            menu.findItem(R.id.menu_connect).setVisible(true);
            menu.findItem(R.id.menu_disconnect).setVisible(false);
        }*/
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            /*case R.id.menu_connect:
                mBluetoothLeService.connect(mDeviceAddress);
                return true;
            case R.id.menu_disconnect:
                mBluetoothLeService.disconnect();
                return true;*/
            case android.R.id.home:
                onBackPressed();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }


}
