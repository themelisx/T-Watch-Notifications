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
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.util.Base64;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.Nullable;

import com.google.gson.Gson;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MainService extends Service {

    // Constants
    private final static String TAG = MainService.class.getSimpleName();
    private static final int ICON_PIXELS = 24;
    private static final int ICON_COMPRESSION_QUALITY = 80;
    private final String LIST_NAME = "NAME";
    private final String LIST_UUID = "UUID";

    public static final String DEVICE_ADDRESS = "device_address";

    // Commands from BT device
    private static final String GET_PACKAGE_ICON = "GET_PKG_ICON=";
    private static final String GET_NOTIFICATION_LIST = "GET_NOTIF_LIST=";

    // Actions
    public final static String NOTIFICATION_ACTION = "com.example.NOTIFICATION_LISTENER_EXAMPLE";
    public final static String GET_NOTIFICATION_INTENT = "com.example.NOTIFICATION_LISTENER_SERVICE_EXAMPLE";

    // Global variables
    private boolean mConnected = false;
    private String mDeviceAddress;
    private BLE_Service mBLEService;
    private NotificationReceiver nReceiver = new NotificationReceiver();
    private ArrayList<ArrayList<BluetoothGattCharacteristic>> mGattCharacteristics = new ArrayList<ArrayList<BluetoothGattCharacteristic>>();
    private BluetoothGattCharacteristic mNotifyCharacteristic;
    private BluetoothGattCharacteristic mWriteCharacteristic;

    public MainService() {
    }

    ///////////////////////
    // Service functions //
    ///////////////////////
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent != null) {
            mDeviceAddress = intent.getStringExtra(DEVICE_ADDRESS);
        } else {
            SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
            mDeviceAddress = sharedPreferences.getString(DEVICE_ADDRESS, "00:00:00:00:00");
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

        return START_STICKY; // Keep the service alive
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

    ///////////////////////
    // Private functions //
    ///////////////////////
    private String getApplicationName(String packageName) {
        try {
            PackageManager pm = getPackageManager();
            return (String) pm.getApplicationLabel(pm.getApplicationInfo(packageName, PackageManager.GET_META_DATA));
        } catch (PackageManager.NameNotFoundException e) {
            return "Unknown app";
        }
    }

    private Drawable getApplicationIcon(String packageName) {
        try {
            PackageManager pm = getPackageManager();
            return pm.getApplicationIcon(pm.getApplicationInfo(packageName, PackageManager.GET_META_DATA));
        } catch (PackageManager.NameNotFoundException e) {
            return null;
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

    private final BroadcastReceiver mGattUpdateReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {

            final String action = intent.getAction();
            if (BLE_Service.ACTION_GATT_CONNECTED.equals(action)) {
                mConnected = true;
                Log.i(TAG, getString(R.string.connected));
            } else if (BLE_Service.ACTION_GATT_DISCONNECTED.equals(action)) {
                mConnected = false;
                Log.i(TAG, getString(R.string.disconnected));
            } else if (BLE_Service.ACTION_GATT_SERVICES_DISCOVERED.equals(action)) {
                // Show all the supported services and characteristics on the user interface.
                displayGattServices(mBLEService.getSupportedGattServices());
            } else if (BLE_Service.ACTION_DATA_AVAILABLE.equals(action)) {

                String s = intent.getStringExtra(BLE_Service.EXTRA_DATA);
                Log.i(TAG, "ESP says: " + s);

                if (s.startsWith(GET_PACKAGE_ICON)) {
                    // Get icon of the requested package name
                    //  (used after "new_notification" as request from the BT device)
                    s = s.replace(GET_PACKAGE_ICON, "");
                    sendApplicationIcon(s);
                } else if (s.startsWith(GET_NOTIFICATION_LIST)) {
                    // Send a broadcast to the Notification Service
                    //  to request the active notification list
                    s = s.replace(GET_NOTIFICATION_LIST, "");
                    Log.d(TAG, "Command: Get Notification List");
                    Log.d(TAG, "Value: " + s);
                    Intent i = new Intent(GET_NOTIFICATION_INTENT);
                    i.putExtra("command", "list");
                    sendBroadcast(i);
                } else {
                    Log.e(TAG, "Unknown command");
                }
            } /*else {
                Log.e(TAG, "Action=" + action);
            }*/
        }
    };

    private void sendApplicationIcon(String packageName) {
        Log.d(TAG, "Get package icon: " + packageName);
        // Get Application icon
        Drawable drawable = getApplicationIcon(packageName);
        if (drawable != null) {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            // Create a bitmap from drawable
            Bitmap bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight(), Bitmap.Config.ARGB_8888);
            // Resize to 24x24
            bitmap = getResizedBitmap(bitmap, ICON_PIXELS, ICON_PIXELS);
            // Compress and convert it to PNG
            bitmap.compress(Bitmap.CompressFormat.JPEG, ICON_COMPRESSION_QUALITY, byteArrayOutputStream);
            // Get the bytes
            byte[] byteArray = byteArrayOutputStream.toByteArray();
            // Convert it to Base64
            String encoded = Base64.encodeToString(byteArray, Base64.DEFAULT);
            // Send it via BT
            Log.d(TAG, "Sending " + encoded.length() + " bytes...");
            sendData("ICON=" + encoded);
        } else {
            Log.e(TAG, "Error loading application icon from package: " + packageName);
        }
    }

    private Bitmap getResizedBitmap(Bitmap bm, int newHeight, int newWidth) {
        // Get the original bitmap dimensions
        int width = bm.getWidth();
        int height = bm.getHeight();
        Matrix matrix = new Matrix();
        // Resize the bitmap
        matrix.postScale(((float) newWidth) / width, ((float) newHeight) / height);
        // Create the new bitmap
        return Bitmap.createBitmap(bm, 0, 0, width, height, matrix, false);
    }

    private void sendData(String data) {

        if (mConnected) {
            if (mWriteCharacteristic != null) {
                byte[] strBytes = data.getBytes();
                mWriteCharacteristic.setValue(strBytes);
                mBLEService.writeCharacteristic(mWriteCharacteristic);

            } else {
                Log.e(TAG, "mWriteCharacteristic is null");
                doDisconnect(null);
            }
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

    private void doDisconnect(View view) {
        if (mBLEService != null) {
            mBLEService.disconnect();
        }
    }

    /**
    // Demonstrates how to iterate through the supported GATT Services/Characteristics.
    // In this sample, we populate the data structure that is bound to the ExpandableListView
    // on the UI.
    */
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

    //////////////////////
    // Internal classes //
    //////////////////////
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
}