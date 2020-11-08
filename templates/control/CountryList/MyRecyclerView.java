package com.artech.masterwatch.countrylist.controls;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.artech.controllers.ViewData;

public class MyRecyclerView extends RecyclerView {
    private final MyRecyclerAdapter mAdapter;

    public MyRecyclerView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);

        mAdapter = new MyRecyclerAdapter(LayoutInflater.from(context));
        setAdapter(mAdapter);

        setLayoutManager(new LinearLayoutManager(context));
    }

    public void update(ViewData data) {
        mAdapter.setData(data);
    }
}
