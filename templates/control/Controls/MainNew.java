package com.artech.masterwatch.controls;

import android.os.Bundle;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;

import com.artech.base.model.Entity;
import com.artech.controllers.ViewData;

import com.artech.masterwatch.MainBase;

public class MainNew extends MainBase {

    @Override
    public void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.main);
    }

    @Override
    protected String getMetadataDataView() {
        return "Controls.Level.Detail";
    }

    @Override
    public void update(ViewData data) {
        if (data.getEntities() == null)
          return;

        Entity entity = data.getSingleEntity();

        EditText edit = (EditText)findViewById(R.id._num);
        edit.setText((String)entity.getProperty("Num"));

    }

    public void onButtonClick(View view) {
        executeEvent("Click");
    }

}
