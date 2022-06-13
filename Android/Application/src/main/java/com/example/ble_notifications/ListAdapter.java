package com.example.ble_notifications;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.example.ble_notifications.R;

import java.util.ArrayList;

// Adapter for holding devices found through scanning.
public class ListAdapter extends BaseAdapter {
    private ArrayList<myMessages> messages;
    private Context context;

    class myMessages {
        int type;
        String msg;
    }

    public ListAdapter(Context context) {
        super();
        messages = new ArrayList<myMessages>();
        this.context = context;

    }

    public void addAndroidMessage(String message) {

        myMessages myMsg = new myMessages();
        myMsg.type = 1;
        myMsg.msg = message;

        messages.add(myMsg);
        notifyDataSetChanged();
    }

    public void addEspMessage(String message) {

        myMessages myMsg = new myMessages();
        myMsg.type = 2;
        myMsg.msg = message;

        messages.add(myMsg);
        notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return messages.size();
    }

    @Override
    public Object getItem(int i) {
        return messages.get(i);
    }

    @Override
    public long getItemId(int i) {
        return i;
    }

    @SuppressLint("MissingPermission")
    @Override
    public View getView(int i, View view, ViewGroup viewGroup) {

        LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);

        ViewHolder viewHolder;
        // General ListView optimization code.
        if (view == null) {
            view = inflater.inflate(R.layout.listitem, null);
            viewHolder = new ViewHolder();
            viewHolder.msg = (TextView) view.findViewById(R.id.msg);
            view.setTag(viewHolder);
        } else {
            viewHolder = (ViewHolder) view.getTag();
        }

        myMessages msg = messages.get(i);
        if (msg.type == 1) {
            viewHolder.msg.setText("Android: " + messages.get(i).msg);
        } else {
            viewHolder.msg.setText("ESP: " + messages.get(i).msg);
        }

        return view;
    }

    class ViewHolder {
        TextView msg;
    }
}


