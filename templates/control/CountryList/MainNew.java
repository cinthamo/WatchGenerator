package com.artech.masterwatch.countrylist;

import android.os.Bundle;
import android.widget.EditText;
import android.widget.TextView;

import com.artech.base.model.Entity;
import com.artech.controllers.ViewData;
import com.artech.masterwatch.countrylist.controls.MyRecyclerView;

import com.artech.masterwatch.MainBase;

public class MainNew extends MainBase {

    @Override
    public void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.main);
    }

    @Override
    protected String getMetadataDataView() {
        return "CountryList.Level.Detail";
    }

    @Override
    protected Integer getDataSourceIndex() { return 0; }

    @Override
    public void update(ViewData data) {
        if (data.getEntities() == null)
          return;

        Entity entity = data.getSingleEntity();

        MyRecyclerView view = (MyRecyclerView)findViewById(R.id.Grid1);
        view.update(data);

    }

}
