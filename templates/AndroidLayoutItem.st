$if(item.s.label)$
$!--- Edit ---!$
    <LinearLayout
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:orientation="vertical">

        <TextView
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:textColor="@android:color/white"
            android:text="$item.s.label$" />

        <EditText
            android:id="@+id/$item.b.id$"
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:textColor="@android:color/white"
            android:inputType="$item.s.inputType$" />

    </LinearLayout>
$elseif(item.s.onClick)$
$!--- Button ---!$
    <Button
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:onClick="$item.s.onClick$"
        android:background="@android:color/black"
        android:textColor="@android:color/white"
        android:text="$item.s.text$" />
$elseif(item.s.text)$
$!--- Text ---!$
    <TextView
        android:id="@+id/$item.b.id$"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:textColor="@android:color/white"
$if(item.s.gravity)$
        android:gravity="$item.s.gravity$"
$endif$
        android:text="$item.s.text$" />
$elseif(item.s.width)$
$!--- Image ---!$
    <com.artech.masterwatch.MyImageView
        android:id="@+id/$item.b.id$"
        android:layout_width="$item.s.width$dip"
        android:layout_height="$item.s.height$dip"
        android:gravity="center" />
$else$
$!--- Grid ---!$
    <$item.s.package$.controls.MyRecyclerView
        android:id="@+id/$item.b.id$"
        android:layout_width="match_parent"
        android:layout_height="match_parent" />
$endif$
