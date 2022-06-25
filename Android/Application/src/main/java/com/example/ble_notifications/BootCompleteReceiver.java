package com.example.ble_notifications;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;

public class BootCompleteReceiver extends BroadcastReceiver {

    private static final String TAG = "BootCompleteReceiver";
    public static final String DEVICE_ADDRESS = "device_address";

    @Override
    public void onReceive(Context context, Intent intent) {

        if (Intent.ACTION_BOOT_COMPLETED.equals(intent.getAction())) {
            Log.i(TAG, "Action BOOT_COMPLETED");

            SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
            String mDeviceAddress = sharedPreferences.getString(DEVICE_ADDRESS, "00:00:00:00:00");

            Intent mainService = new Intent(context, MainService.class);
            mainService.putExtra(DEVICE_ADDRESS, mDeviceAddress);
            context.startService(mainService);
        }
    }

}
