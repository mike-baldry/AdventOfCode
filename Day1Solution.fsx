(*
    Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

    Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

    For example, suppose your expense report contained the following:

    1721 979 366 299 675 1456 In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

    Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?
*)

let input = 
    [
        1939; 1585; 1712; 1600; 1370; 1447; 1247; 1446; 1323; 1713; 1277; 1946; 1677; 1428; 1231; 1481; 1976; 1709; 1508; 1668; 1302; 77; 
        1351; 1605; 1999; 1982; 1583; 1756; 1957; 1624; 1745; 1938; 1784; 1403; 1642; 1691; 569; 1762; 1555; 1937; 1383; 1897; 1334; 1965;
        1683; 1475; 1776; 1791; 1707; 1987; 1233; 1416; 1769; 1345; 1874; 1255; 1744; 1944; 1404; 1360; 1304; 1417; 1977; 1656; 790; 1788;
        1353; 1296; 1673; 1810; 1684; 1742; 1425; 1887; 1444; 1352; 1229; 1414; 1493; 1402; 1947; 1669; 1412; 1531; 1474; 1637; 1314; 1607;
        1829; 1923; 1949; 1757; 1307; 1714; 1748; 1550; 1372; 1615; 1235; 1272; 1408; 1749; 1687; 1613; 1528; 1561; 341; 1308; 1660; 1667;
        1313; 1991; 1675; 1394; 1704; 1303; 1440; 1592; 1857; 1752; 1839; 1397; 1699; 1426; 1878; 1759; 1814; 1096; 372; 1596; 1500; 1774;
        1627; 1696; 1851; 1020; 1819; 1292; 1616; 1672; 1279; 1543; 1526; 1682; 1568; 1582; 1921; 922; 1773; 1482; 1238; 1973; 1517; 1909;
        409; 1634; 1468; 1445; 1801; 1631; 1407; 1820; 1603; 1495; 1333; 1241; 1849; 82; 1339; 1413; 90; 1662; 1291; 1740; 1340; 1365; 2003;
        1546; 1621; 1650; 1518; 1807; 1382; 1433; 1968; 1940; 1986; 1437; 1651; 1237; 1862; 1409; 1200; 2002; 2009; 1735; 1487; 1706; 1643; 1505; 
    ]

let part1 () = 
    seq {
        for x in input do 
            let y = 2020 - x
            if input |> List.contains y then yield sprintf $"Answer: x: {x} y: {y}, x * y : {x * y}"
    }
    |> Seq.head

let part2 () =
    seq {
        for x in input do
            for y in input do 
                let z = 2020 - (x + y)
                if input |> List.contains z then yield $"Answer: x: {x} y: {y}, z: {z}, x * y * z : {x * y * z}"
    }
    |> Seq.head