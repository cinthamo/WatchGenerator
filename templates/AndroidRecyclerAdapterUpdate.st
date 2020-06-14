$if(update.isText)$
$!--- Text ---!$
        TextView view = (TextView)viewHolder.itemView.findViewById(R.id.$update.id$);
        view.setText((String)entity.getProperty("$update.property$"));
$elseif(update.isEdit)$
$!--- Edit ---!$
        EditText edit = (EditText)viewHolder.itemView.findViewById(R.id.$update.id$);
        edit.setText((String)entity.getProperty("$update.property$"));
$elseif(update.isImage)$
$!--- Image ---!$
        MyImageView imageView = (MyImageView)viewHolder.itemView.findViewById(R.id.$update.id$);
        String imageUri = (String)entity.getProperty("$update.property$");
        String url = MyApplication.getApp().UriMaker.MakeImagePath(imageUri);
        imageView.setImageTag(url);
        imageLoader.DisplayImage(url, imageView, true, false);
$else$
$!--- Nested Grids not supported ---!$
$endif$
