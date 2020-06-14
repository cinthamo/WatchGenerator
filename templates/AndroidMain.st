package $d.package$;

import android.os.Bundle;
$if(d.events)$
import android.view.View;
$endif$$if(d.update)$
import android.widget.EditText;
import android.widget.TextView;

import com.artech.base.model.Entity;
import com.artech.controllers.ViewData;
$if(d.hasGrid)$
import $d.package$.controls.MyRecyclerView;
$endif$$endif$
import com.artech.masterwatch.MainBase;

public class MainNew extends MainBase {

    @Override
    public void onCreate(Bundle b) {
        super.onCreate(b);
        setContentView(R.layout.main);
    }
$if(d.dataView)$

    @Override
    protected String getMetadataDataView() {
        return "$d.dataView$";
    }
$endif$$if(d.sourceIndex)$

    @Override
    protected Integer getDataSourceIndex() { return $d.sourceIndex$; }
$endif$$if(d.update)$

    @Override
    public void update(ViewData data) {
        if (data.getEntities() == null)
          return;

        Entity entity = data.getSingleEntity();
$d.update: {update|$AndroidMainUpdate(update)$}$
    }
$endif$$if(d.events)$
$d.events: {event|$AndroidMainEvent(event)$}$
$endif$
}
