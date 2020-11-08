package $d.packageA$.controls;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.EditText;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.artech.application.MyApplication;
import com.artech.base.model.Entity;
import com.artech.controllers.ViewData;
import com.artech.masterwatch.MyImageView;
import $d.packageA$.R;
import com.fedorvlasov.lazylist.ImageLoader;

import java.util.ArrayList;

public class MyRecyclerAdapter extends RecyclerView.Adapter<MyRecyclerAdapter.ViewHolder> {
    private final LayoutInflater mInflater;
    private final ImageLoader mImageLoader;
    private ArrayList<Entity> mEntities;

    public MyRecyclerAdapter(LayoutInflater inflater) {
        mInflater = inflater;
        mImageLoader = new ImageLoader(inflater.getContext());
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        public ViewHolder(View view) {
            super(view);
        }
    }

    public void setData(ViewData data) {
        mEntities = data.getEntities();
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public MyRecyclerAdapter.ViewHolder onCreateViewHolder(@NonNull ViewGroup viewGroup, int viewType) {
        View view = mInflater.inflate(R.layout.line, viewGroup, false);
        return new MyRecyclerAdapter.ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(MyRecyclerAdapter.ViewHolder viewHolder, int position) {
        Entity entity = mEntities.get(position);
$d.bindView: {update|$AndroidRecyclerAdapterUpdate(update)$}$
    }

    @Override
    public int getItemCount() {
        return mEntities == null ? 0 : mEntities.size();
    }
}
