import java.util.ArrayList;

/*
 * @lc app=leetcode id=25 lang=java
 *
 * [25] Reverse Nodes in k-Group
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode(int x) { val = x; }
 * }
 */
class Solution {
    public ListNode reverseKGroup(ListNode head, int k) {
        if (k <= 0) return head;
        ListNode newHead = null, newTail = null, start = head, cur = head, tmp;
        int count = 1;

        while (cur != null) {
            if (count == k) {
                tmp = cur.next;
                cur.next = null;
                List<ListNode> listk = reverse(start);
                if (newHead == null) {
                    newHead = listk.get(0);
                    newTail = listk.get(1);
                } else {
                    newTail.next = listk.get(0);
                    newTail = listk.get(1);
                }

                start = cur = tmp;
                count = 1;
            } else {
                cur = cur.next;
                ++count;
            }
        }

        if (newTail != null) newTail.next = start;
        else newHead = head;

        return newHead;
    }

    private List<ListNode> reverse(ListNode head) {
        ListNode newTail = head, newHead = null, tmp;
        List<ListNode> ret = new ArrayList<ListNode>();

        while (head != null) {
            tmp = head.next;
            head.next = newHead;
            newHead = head;
            head = tmp;
        }

        ret.add(newHead);
        ret.add(newTail);
        return ret;
    }
}
// 35mins
// @lc code=end

