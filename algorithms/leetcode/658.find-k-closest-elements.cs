/*
 * @lc app=leetcode id=658 lang=csharp
 *
 * [658] Find K Closest Elements
 */

// @lc code=start
public class Solution {
    public IList<int> FindClosestElements(int[] arr, int k, int x) {
        int l, r;
        int posi = FindPosition(arr, x);

        if (posi - k / 2 < 0) {
            l = 0;
            r = k - 1;
        } else if (posi + (k - 1) / 2 > arr.Length - 1) {
            l = arr.Length - k;
            r = arr.Length - 1;
        } else {
            l = posi - k / 2;
            r = posi + (k - 1) / 2;
        }

        while (r < arr.Length - 1 && x - arr[l] > arr[r + 1] - x) {
            ++l; ++r;
        }
        while (l > 0 && arr[r] - x >= x - arr[l - 1]) {
            --l; --r;
        }

        return new List<int>(arr).GetRange(l, k);
    }

    private int FindPosition(int[] arr, int x) {
        int l = 0, r = arr.Length - 1;
        int mid;

        while (l <= r) {
            mid = (l + r) / 2;
            if (arr[mid] == x && (mid == 0 || arr[mid-1] < x)) return mid;
            if (arr[mid] < x) l = mid + 1;
            else r = mid - 1;
        }

        if (r < 0) return 0;
        else if (l >= arr.Length) return arr.Length - 1;
        return x - arr[r] < arr[l] - x ? r : l;
    }
}
// @lc code=end

