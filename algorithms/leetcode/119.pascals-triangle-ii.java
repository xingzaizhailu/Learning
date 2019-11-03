import java.util.ArrayList;
import java.util.List;

/*
 * @lc app=leetcode id=119 lang=java
 *
 * [119] Pascal's Triangle II
 */

// @lc code=start
class Solution {
    public List<Integer> getRow(int rowIndex) {
        List<Integer> ret = new ArrayList<>();
        ret.add(1);

        while (rowIndex > 0) {
            List<Integer> tmp = new ArrayList<>();

            for (int i = 0; i < ret.size(); ++i) {
                if (i == 0) tmp.add(ret.get(i));
                if (i < ret.size() - 1) {
                    tmp.add(ret.get(i) + ret.get(i + 1));
                } else {
                    tmp.add(ret.get(i));
                }
            }
            ret = tmp;
            --rowIndex;
        }

        return ret;
    }
}
// @lc code=end

