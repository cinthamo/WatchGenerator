<?xml version="1.0" encoding="utf-8"?>
$if(d.useScroll)$
<ScrollView
$else$
<LinearLayout
$endif$
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
$if(d.isMain)$
    android:layout_height="match_parent"
    android:background="@android:color/black"
$elseif(d.layoutHeight)$
    android:layout_height="$d.layoutHeight$dip"
$else$
    android:layout_height="wrap_content"
$endif$
$if(d.isVertical)$
    android:orientation="vertical">
$else$
    android:orientation="horizontal">
$endif$
$d.items: {item|$AndroidLayoutItem(item)$}$
$if(d.useScroll)$
</ScrollView>
$else$
</LinearLayout>
$endif$
