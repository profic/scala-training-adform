package com.adform.task;

import com.adform.tree.openjdk_interval_rb_tree.*;

import java.util.Comparator;
import java.util.List;

/**
 * Created by vladislav.molchanov on 18.02.2016.
 */
public class TestOpenJdk {

    public static void main(String[] args) {

        IntervalTree tree = new IntervalTree(new Comparator<Comparable>() {
            @Override
            public int compare(Comparable o1, Comparable o2) {
                return o1.compareTo(o2);
            }
        });

        tree.insert(new Interval(1, 10), "111");
        tree.insert(new Interval(5, 11), "222");
        tree.insert(new Interval(1, 4), "333");
        tree.insert(new Interval(6, 9), "333");

        List<IntervalNode> res = tree.findAllNodesIntersecting(new Interval(2, 2));
        for (IntervalNode r : res) {
            Object data = r.getData();
            System.out.println(data);
        }

    }

}
