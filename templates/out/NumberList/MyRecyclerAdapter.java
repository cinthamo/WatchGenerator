package com.artech.masterwatch.numberlist.controls;

import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.EditText;

import com.artech.application.MyApplication;
import com.artech.base.model.Entity;
import com.artech.controllers.ViewData;
import com.artech.masterwatch.MyImageView;
import com.artech.masterwatch.numberlist.R;
import com.fedorvlasov.lazylist.ImageLoader;

import java.util.ArrayList;

public class MyRecyclerAdapter extends RecyclerView.Adapter<MyRecyclerAdapter.ViewHolder> {
    private LayoutInflater mInflater;
    private ArrayList<Entity> mEntities;

    public MyRecyclerAdapter(LayoutInflater inflater) {
        mInflater = inflater;
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ViewHolder(View view) {
            super(view);
        }
    }

    public void setData(ViewData data) {
        mEntities = data.getEntities();
        notifyDataSetChanged();
    }

    @Override
    public MyRecyclerAdapter.ViewHolder onCreateViewHolder(ViewGroup viewGroup, int viewType) {
        View view = mInflater.inflate(R.layout.line, viewGroup, false);
        return new MyRecyclerAdapter.ViewHolder(view);
    }

    private ImageLoader imageLoader = new ImageLoader();

    @Override
    public void onBindViewHolder(MyRecyclerAdapter.ViewHolder viewHolder, int position) {
        Entity entity = mEntities.get(position);

        TextView view = (TextView)viewHolder.itemView.findViewById(R.id._n);
        view.setText((String)entity.getProperty("N"));

    }

    @Override
    public int getItemCount() {
        return mEntities == null ? 0 : mEntities.size();
    }
}