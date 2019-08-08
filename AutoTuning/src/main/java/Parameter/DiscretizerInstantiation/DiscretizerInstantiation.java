package Parameter.DiscretizerInstantiation;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public interface DiscretizerInstantiation<T extends Discretizer> {

    T[] getAllDiscretizers();

    default List<T> selectRandom(int count) {
        List<T> arrayList = new ArrayList<>(Arrays.asList(getAllDiscretizers()));
        if (arrayList.size() < count)
            throw new IllegalArgumentException("Less elements than expected");
        Collections.shuffle(arrayList);
        return arrayList.subList(0, count);
    }
}
