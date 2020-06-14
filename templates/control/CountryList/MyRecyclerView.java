package com.artech.masterwatch.countrylist.controls;

/**
 * Created by Cristian on 19/8/17.
 */

import android.content.Context;
import android.support.annotation.Nullable;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.AttributeSet;
import android.view.LayoutInflater;

import com.artech.controllers.ViewData;

public class MyRecyclerView extends RecyclerView {
    private MyRecyclerAdapter mAdapter;
    private LayoutManager mLayoutManager;

    public MyRecyclerView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);

        mAdapter = new MyRecyclerAdapter(LayoutInflater.from(context));
        setAdapter(mAdapter);

        mLayoutManager = new LinearLayoutManager(context);
        setLayoutManager(mLayoutManager);
    }

    public void update(ViewData data) {
        mAdapter.setData(data);
    }
}
