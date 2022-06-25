package com.example.ble_notifications;

import android.app.Notification;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.service.notification.NotificationListenerService;
import android.service.notification.StatusBarNotification;
import android.text.SpannableString;
import android.util.Log;

import com.google.gson.Gson;

/* Notification listener service
handles obtaining the notifications from the android device and formats the results
into a string which can be read in MainActivity

This file follows the example provided by github user kpbird which can be found here:
        https://github.com/kpbird/NotificationListenerService-Example
 */
public class NotificationService extends NotificationListenerService {

    // Constants
    private final String TAG = this.getClass().getSimpleName();

    public final static String NOTIFICATION_ACTION = "com.example.NOTIFICATION_LISTENER_EXAMPLE";
    public final static String GET_NOTIFICATION_INTENT = "com.example.NOTIFICATION_LISTENER_SERVICE_EXAMPLE";

    // Global variables
    private NLServiceReceiver nlServiceReceiver;

    // Service functions
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        // Keep the service alive
        return START_STICKY;
    }

    @Override
    public void onCreate() {
        super.onCreate();

        nlServiceReceiver = new NLServiceReceiver();
        IntentFilter filter = new IntentFilter();
        filter.addAction(GET_NOTIFICATION_INTENT);
        registerReceiver(nlServiceReceiver, filter);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        unregisterReceiver(nlServiceReceiver);
    }

    @Override
    public void onNotificationPosted(StatusBarNotification sbn) {
        Log.i(TAG, "onNotificationPosted");

        Notification n = sbn.getNotification();
        if (n.category != null && !n.category.equalsIgnoreCase("sys")) {
            Intent intent = new Intent(NOTIFICATION_ACTION);
            intent.putExtra("type", "new_notification");
            intent.putExtra("package", sbn.getPackageName());
            intent.putExtra("id", sbn.getId());
            intent.putExtra("category", n.category);
            intent.putExtra("title", Trim(n.extras.getString(Notification.EXTRA_TITLE, "")));

            if (n.extras.get(Notification.EXTRA_TEXT) instanceof SpannableString) {
                intent.putExtra("text", Trim(n.extras.get(Notification.EXTRA_TEXT).toString()));
            } else {
                intent.putExtra("text", Trim(n.extras.getString(Notification.EXTRA_TEXT)));
            }
            if (n.category.equals("email")) {
                intent.putExtra("sub_text", Trim(n.extras.getString(Notification.EXTRA_SUB_TEXT, ""))); // (email) account
            }
            sendBroadcast(intent);
        }
    }

    @Override
    public void onNotificationRemoved(StatusBarNotification sbn) {
        Log.i(TAG, "onNotificationRemoved");

        Notification n = sbn.getNotification();
        if (n.category != null) {
            Intent intent = new Intent(NOTIFICATION_ACTION);
            intent.putExtra("id", sbn.getId());
            intent.putExtra("type", "remove_notification");
            sendBroadcast(intent);
        }
    }

    // Private functions
    private String Trim(String data) {
        if (data.length() > 20) {
            return data.substring(0, 19) + "...";
        } else {
            return data;
        }
    }

    // Internal classes
    class NLServiceReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent incomingIntent) {
            Log.i(TAG, "NLServiceReceiver: onReceive");
            Intent intent;

            if (incomingIntent.getStringExtra("command").equals("clearall")) {
                Log.i(TAG, "Clear All");
                NotificationService.this.cancelAllNotifications();

            } else if (incomingIntent.getStringExtra("command").equals("list")) {
                Log.i(TAG, "Reading notifications list");

                NotificationList notificationList = new NotificationList();

                for (StatusBarNotification sbn : NotificationService.this.getActiveNotifications()) {

                    Notification n = sbn.getNotification();
                    if (n.category != null && !n.category.equalsIgnoreCase("sys")) {
                        NotificationBundle notificationBundle = new NotificationBundle();
                        notificationBundle.id = sbn.getId();
                        notificationBundle.pName = sbn.getPackageName();
                        notificationBundle.category = n.category;
                        notificationBundle.title = n.extras.getString(Notification.EXTRA_TITLE, "");

                        if (n.extras.get(Notification.EXTRA_TEXT) instanceof SpannableString) {
                            notificationBundle.text = n.extras.get(Notification.EXTRA_TEXT).toString();
                        } else {
                            notificationBundle.text = n.extras.getString(Notification.EXTRA_TEXT);
                        }
                        if (n.category.equals("email")) {
                            notificationBundle.subText = n.extras.getString(Notification.EXTRA_SUB_TEXT, ""); // (email) account
                        }
                        notificationList.nBundleList.add(notificationBundle);
                    }
                }

                notificationList.count = notificationList.nBundleList.size();

                intent = new Intent(NOTIFICATION_ACTION);
                intent.putExtra("type", "list_notification");
                intent.putExtra("data", new Gson().toJson(notificationList));
                sendBroadcast(intent);
            }
        }
    }
}