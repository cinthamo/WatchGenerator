$if(update.isText)$
$!--- Text ---!$
        TextView view = viewHolder.itemView.findViewById(R.id.$update.id$);
        view.setText((String)entity.getProperty("$update.property$"));
$elseif(update.isEdit)$
$!--- Edit ---!$
        EditText edit = viewHolder.itemView.findViewById(R.id.$update.id$);
        edit.setText((String)entity.getProperty("$update.property$"));
$elseif(update.isImage)$
$!--- Image ---!$
        MyImageView imageView = viewHolder.itemView.findViewById(R.id.$update.id$);
        String imageUri = (String)entity.getProperty("$update.property$");
        String url = MyApplication.getApp().UriMaker.getImageUrl(imageUri);
        imageView.setImageTag(url);
        mImageLoader.displayImage(url, imageView, true, false, false);
$else$
$!--- Nested Grids not supported ---!$
$endif$
