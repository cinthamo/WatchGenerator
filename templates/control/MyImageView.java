package com.artech.masterwatch;

import android.content.Context;
import android.util.AttributeSet;

import com.artech.base.metadata.theme.ThemeClassDefinition;
import com.artech.controls.common.IViewDisplayImage;

public class MyImageView extends android.support.v7.widget.AppCompatImageView implements IViewDisplayImage {
    public MyImageView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }
    private String mImageTag;

    @Override
    public String getImageTag() {
        return mImageTag;
    }

    @Override
    public void setImageTag(String tag) {
        mImageTag = tag;
    }

    @Override
    public ThemeClassDefinition getThemeClass() {
        return null;
    }
}
