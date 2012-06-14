package sodium;

import java.util.List;
import java.util.ArrayList;

public abstract class Listener {
    abstract void unlisten();
     
    private List<Listener> finalizers = new ArrayList<Listener>();

    Listener addCleanup(Listener cleanup)
    {
        finalizers.add(cleanup);
        return this;
    }

	@Override
	protected void finalize() throws Throwable {
		for (Listener l : finalizers)
			l.unlisten();
	}
}
