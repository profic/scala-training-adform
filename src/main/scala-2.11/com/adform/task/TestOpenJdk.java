package com.adform.task;

import com.adform.tree.openjdk_interval_rb_tree.*;

import java.net.InetAddress;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
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

        Files.lines(Paths.get(TestOpenJdk.class.getResource("/ranges.tsv").toURI())).map(l -> l.split("-|\t")).map(arr -> {
            try {
                long begin = ipToLong(InetAddress.getByName(arr[0]));
                long end = ipToLong(InetAddress.getByName(arr[1]));
                return new Interval(begin, end);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }).forEach(interval -> tree.insert(interval, "111"));

        Long key = ipToLong(InetAddress.getByName("92.173.0.104"));

        List<IntervalNode> res = tree.findAllNodesIntersecting(new Interval(key, key));
        System.out.println(tree.getLookupCnt());
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
