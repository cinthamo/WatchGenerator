package com.artech.masterwatch;

import android.app.Activity;
import android.app.ProgressDialog;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.view.View;

import com.artech.actions.ActionExecution;
import com.artech.actions.ActionFactory;
import com.artech.actions.ActionParameters;
import com.artech.actions.CompositeAction;
import com.artech.actions.UIContext;
import com.artech.activities.ActivityController;
import com.artech.activities.IntentParameters;
import com.artech.app.ComponentId;
import com.artech.app.ComponentParameters;
import com.artech.application.MyApplication;
import com.artech.base.metadata.ActionDefinition;
import com.artech.base.metadata.IDataSourceDefinition;
import com.artech.base.metadata.IDataViewDefinition;
import com.artech.base.metadata.IViewDefinition;
import com.artech.base.metadata.StructureDefinition;
import com.artech.base.metadata.enums.Connectivity;
import com.artech.base.metadata.enums.DisplayModes;
import com.artech.base.metadata.layout.LayoutDefinition;
import com.artech.base.metadata.loader.LoadResult;
import com.artech.base.model.Entity;
import com.artech.base.services.Services;
import com.artech.controllers.DataViewController;
import com.artech.controllers.IDataSourceBoundView;
import com.artech.controllers.IDataSourceController;
import com.artech.controllers.IDataViewController;
import com.artech.controllers.RefreshParameters;
import com.artech.controllers.ViewData;
import com.artech.fragments.IDataView;
import com.artech.ui.Anchor;

public class MainBase extends Activity implements IDataView, IDataSourceBoundView {
    private ProgressDialog mProgressDialog;
    private ActivityController mController;
    private LayoutDefinition mLayout;

    @Override
    public void onCreate(Bundle b) {
        super.onCreate(b);
        if (!Services.Application.isLoaded()) {
            loadMetadata();
        }
    }

    // Metadata
    private void loadMetadata() {
        mProgressDialog = ProgressDialog.show(this, getResources().getText(com.artech.R.string.GXM_Loading), getResources().getText(com.artech.R.string.GXM_PleaseWait), true);
        Thread thread = new Thread(null, doBackgroundProcessing, "Background");
        thread.start();
    }

    private Handler mHandler = new Handler();

    private final Runnable doBackgroundProcessing = new Runnable() {
        @Override
        public void run() {
            loadApplication();
        }
    };

    private void loadApplication() {
        //If app data is not loaded, load it.
        if (!Services.Application.isLoaded()) {
            LoadResult loadResult;
            try {
                // Load the Application.
                loadResult = Services.Application.initialize();
            } catch (Exception ex) {
                // Uncaught exception, possibly "out of memory".
                loadResult = LoadResult.error(ex);
            }
            if (loadResult.getCode() != LoadResult.RESULT_OK) {
                Services.Log.Error("Metadata could not be load.", "Message: " + loadResult.getErrorMessage());
            }
            mHandler.post(new AfterLoadRunnable(loadResult));
        }
    }

    private class AfterLoadRunnable implements Runnable {
        private final LoadResult mResult;

        private AfterLoadRunnable(LoadResult result) {
            mResult = result;
        }

        @Override
        public void run() {
            mProgressDialog.dismiss();
            loadController();
        }
    }

    // Execute event
    protected void executeEvent(String eventName) {
        IViewDefinition viewDefinition = MyApplication.getApp().getMain();
        ActionDefinition actionDefinition = viewDefinition.getEvent(eventName);
        if (actionDefinition != null) {
            Entity entity = new Entity(StructureDefinition.EMPTY); // Faltan las variables, ver DashboardFragment
            ActionParameters actionParameters = new ActionParameters(entity);
            CompositeAction action = ActionFactory.getAction(getUIContext(), actionDefinition, actionParameters);
            ActionExecution exec = new ActionExecution(action);
            exec.executeAction();
        }
    }

    // To Override
    protected String getMetadataDataView() { return null; }
    protected Integer getDataSourceIndex() { return 0; }

    // Controller
    private void loadController() {
        if (getMetadataDataView() == null)
            return; // No data

        Intent intent = getIntent();
        intent.putExtra(IntentParameters.DataView, getMetadataDataView());
        intent.putExtra(IntentParameters.Connectivity, Connectivity.Offline);
        mController = new ActivityController(this);
        mController.initializeFrom(intent);
        ComponentParameters mainParams = mController.getModel().getMain().getParams();
        DataViewController controller = (DataViewController)mController.getController(getUIContext(), this, ComponentId.ROOT, mainParams);
        mLayout = controller.getDefinition().getLayoutForMode(DisplayModes.VIEW);
        controller.attachDataController(this);
        controller.onResume();
    }

    // IDataView
    @Override
    public IViewDefinition getDefinition() {
        return null;
    }

    @Override
    public short getMode() {
        return 0;
    }

    @Override
    public LayoutDefinition getLayout() {
        return null;
    }

    @Override
    public IDataViewController getController() {
        return null;
    }

    @Override
    public void runAction(ActionDefinition action, Anchor anchor) {

    }

    @Override
    public UIContext getUIContext() {
        return new UIContext(this, null, getRootView(), Connectivity.Offline);
    }

    @Override
    public Entity getContextEntity() {
        return null;
    }

    @Override
    public View getRootView() {
        return findViewById(android.R.id.content);
    }


    // IDataSourceBoundView
    @Override
    public String getDataSourceId() {
        return null;
    }

    @Override
    public IDataSourceDefinition getDataSource() {
        IDataViewDefinition dataViewDefinition = (IDataViewDefinition)MyApplication.getApp().getMain();
        return dataViewDefinition.getDataSources().get(getDataSourceIndex());
    }

    @Override
    public String getDataSourceMember() {
        return null;
    }

    @Override
    public int getDataSourceRowsPerPage() {
        return 0;
    }

    @Override
    public void setController(IDataSourceController controller) {
    }

    @Override
    public boolean isActive() {
        return false;
    }

    @Override
    public void update(ViewData data) {
    }

    @Override
    public void onBeforeRefresh(RefreshParameters params) {
    }

    @Override
    public boolean needsMoreData() {
        return true;
    }

    @Override
    public void setActive(boolean value) {
    }

    @Override
    public boolean isDataReady() {
        return false;
    }

    @Override
    public void refreshData(RefreshParameters params) {
    }
}