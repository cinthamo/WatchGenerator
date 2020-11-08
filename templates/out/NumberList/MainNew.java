package com.artech.masterwatch.numberlist;

import android.os.Bundle;
import android.widget.EditText;
import android.widget.TextView;

import com.artech.base.model.Entity;
import com.artech.controllers.ViewData;
import com.artech.masterwatch.numberlist.controls.MyRecyclerView;

import com.artech.masterwatch.MainBase;

public class MainNew extends MainBase {

    @Override
    public void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.main);
    }

    @Override
    protected String getMetadataDataView() {
        return "NumberList.Level.Detail";
    }

    @Override
    protected Integer getDataSourceIndex() { return 1; }

    @Override
    public void update(ViewData data) {
        if (data.getEntities() == null)
          return;

        Entity entity = data.getSingleEntity();

        MyRecyclerView view = findViewById(R.id.Grid1);
        view.update(data);

    }

}