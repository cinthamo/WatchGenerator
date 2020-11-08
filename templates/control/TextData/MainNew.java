package com.artech.masterwatch.textdata;

import android.os.Bundle;
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
        return "TextData.Level.Detail";
    }

    @Override
    public void update(ViewData data) {
        if (data.getEntities() == null)
          return;

        Entity entity = data.getSingleEntity();

        TextView view = findViewById(R.id._hi);
        view.setText((String)entity.getProperty("Hi"));

    }

}
