package com.adform.task;

import com.adform.tree.openjdk_interval_rb_tree.*;

import java.io.IOException;
import java.net.InetAddress;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * Created by vladislav.molchanov on 18.02.2016.
 */
public class TestOpenJdk {

    public static void main(String[] args) throws Exception {

        IntervalTree tree = new IntervalTree(new Comparator<Comparable>() {
            @Override
            public int compare(Comparable o1, Comparable o2) {
                return o1.compareTo(o2);
            }
        });

        List<Object> s = new ArrayList<>(Arrays.asList(1,2));

        tree.insert(new Interval(1, 4), "");
        tree.insert(new Interval(4, 11), "");
        tree.insert(new Interval(5, 10), "");
        tree.insert(new Interval(3, 9), "");
        tree.insert(new Interval(2, 5), "");
        tree.insert(new Interval(0, 12), "");
        tree.findAllNodesIntersecting(new Interval(6, 6));
        System.out.println(tree.getLookupCnt());

        testIps();
    }

    private static void testIps() throws IOException, URISyntaxException {
        IntervalTree tree = new IntervalTree(new Comparator<Comparable>() {
            @Override
            public int compare(Comparable o1, Comparable o2) {
                return o1.compareTo(o2);
            }
        });

        Files.lines(Paths.get(TestOpenJdk.class.getResource("/ranges copy.tsv").toURI())).map(l -> l.split("-|\t")).forEach(arr -> {
            try {
                long begin = ipToLong(InetAddress.getByName(arr[0]));
                long end = ipToLong(InetAddress.getByName(arr[1]));
                tree.insert(new Interval(begin, end), arr[2]);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });

        Long key = ipToLong(InetAddress.getByName("92.173.0.104"));

        long start = System.nanoTime();
        List<IntervalNode> res = tree.findAllNodesIntersecting(new Interval(key, key));
        System.out.println(System.nanoTime() - start);
//        System.out.println(tree.getLookupCnt());
    }

    private static Long ipToLong(InetAddress ip) {
        byte[] octets = ip.getAddress();
        long result = 0L;

        for (byte octet : octets) {
            result <<= 8;
            result |= octet & 0xff;
        }
        return result;
    }

}
