$if(update.isText)$
$!--- Text ---!$
        TextView view = findViewById(R.id.$update.id$);
        view.setText((String)entity.getProperty("$update.property$"));
$elseif(update.isEdit)$
$!--- Edit ---!$
        EditText edit = findViewById(R.id.$update.id$);
        edit.setText((String)entity.getProperty("$update.property$"));
$else$
$!--- Grid ---!$
        MyRecyclerView view = findViewById(R.id.$update.id$);
        view.update(data);
$endif$
