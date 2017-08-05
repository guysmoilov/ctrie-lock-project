package ctrielock;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

/**
 * The only point of this class is dealing with the {@link #prev} field - which we won't need any more after switching
 * to locks.
 * After removing the prev, the point of this class will be to differentiate between {@link INode} and all other nodes,
 * which can be contained in {@link INodeBase#mainnode}
 */
public abstract class MainNode<K, V> extends BasicNode {

    @Deprecated
    public static final AtomicReferenceFieldUpdater<MainNode, MainNode> updater =
            AtomicReferenceFieldUpdater.newUpdater(MainNode.class, MainNode.class, "prev");

    @Deprecated
    public volatile MainNode<K, V> prev = null;

    @Deprecated
    public boolean CAS_PREV(MainNode<K, V> oldval, MainNode<K, V> nval) {
        return updater.compareAndSet(this, oldval, nval);
    }

    @Deprecated
    public void WRITE_PREV(MainNode<K, V> nval) {
        updater.set(this, nval);
    }
}