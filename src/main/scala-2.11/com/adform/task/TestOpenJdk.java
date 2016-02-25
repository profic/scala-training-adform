package com.adform.task;

import com.adform.tree.openjdk_interval_rb_tree.*;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.io.IOException;
import java.net.InetAddress;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Created by vladislav.molchanov on 18.02.2016.
 */
public class TestOpenJdk {

    static IntervalTree tree;
    static Long key;
    static List<Object[]> parsed;

    static {
        try {

            tree = new IntervalTree(new Comparator<Comparable>() {
                @Override
                public int compare(Comparable o1, Comparable o2) {
                    return o1.compareTo(o2);
                }
            });

            parsed = Files.lines(Paths.get(TestOpenJdk.class.getResource("/ranges.tsv").toURI())).map(l -> l.split("-|\t"))
                    .map(arr -> {
                        try {
                            Long begin = ipToLong(InetAddress.getByName(arr[0]));
                            Long end = ipToLong(InetAddress.getByName(arr[1]));
                            String network = arr[2];
                            return new Object[]{begin, end, network};
                        } catch (UnknownHostException e) {
                            throw new RuntimeException(e);
                        }
                    })
                    .collect(Collectors.toList());

            parsed.forEach(arr -> {
                try {
                    Long begin = (Long) arr[0];
                    Long end = (Long) arr[1];
                    String data = (String) arr[2];
                    tree.insert(new Interval(begin, end), data);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            });

            key = ipToLong(InetAddress.getByName("92.173.0.104"));

            long start = System.nanoTime();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

    }

    public static void main(String[] args) throws Exception {
        Options opt = new OptionsBuilder()
                .include(TestOpenJdk.class.getSimpleName())
                .forks(1)
                .build();
        new Runner(opt).run();
    }

    //    @Benchmark
    public static void doSearch() {
        List<IntervalNode> res = tree.findAllNodesIntersecting(new Interval(key, key));
    }

    @Benchmark
    public static void create() {
        IntervalTree t = new IntervalTree(new Comparator<Comparable>() {
            @Override
            public int compare(Comparable o1, Comparable o2) {
                return o1.compareTo(o2);
            }
        });
        parsed.forEach(arr -> {
            try {
                Long begin = (Long) arr[0];
                Long end = (Long) arr[1];
                String data = (String) arr[2];
                tree.insert(new Interval(begin, end), data);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
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
