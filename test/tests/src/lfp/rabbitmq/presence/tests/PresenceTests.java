//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
package lfp.rabbitmq.presence.tests;

import com.rabbitmq.client.GetResponse;
import com.rabbitmq.client.LongString;
import com.rabbitmq.client.test.BrokerTestCase;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 */
public class PresenceTests extends BrokerTestCase {
    final String xname = "Presence exchange for testing"; /* Presence exchange name */
    final String xtype = "x-presence";
    final String listenKey = "";
    final String otherKey = "other";

    String lq1, lq2; /* Listen queues */
    String oq1, oq2; /* Other queues */

    @Override public void createResources() throws IOException {
        channel.exchangeDeclare(xname, xtype);

        lq1 = channel.queueDeclare().getQueue();
        lq2 = channel.queueDeclare().getQueue();
        oq1 = channel.queueDeclare().getQueue();
        oq2 = channel.queueDeclare().getQueue();

        listenBind(lq1);
        assertEmpty(lq1);
    }

    public void listenBind(String whichListenQueue) throws IOException {
        listenBind(whichListenQueue, true);
    }

    public void listenBind(String whichListenQueue, boolean wantInitialSummary) throws IOException {
        Map<String, Object> args = null;
        if (!wantInitialSummary) {
            args = new HashMap<String, Object>();
            args.put("x-presence-exchange-summary", false);
        }
        channel.queueBind(whichListenQueue, xname, listenKey, args);
    }

    @Override public void releaseResources() throws IOException {
        channel.exchangeDelete(xname);
    }

    public void assertEmpty(String qname) throws IOException {
        assertNull(basicGet(qname));
    }

    String getHeaderString(Map<String, Object> headers, String key) {
        LongString s = (LongString) headers.get(key);
        if (s == null) return null;
        return s.toString();
    }

    public Map<String, Object> getPresenceHeaders(String qname) throws IOException {
        GetResponse r = basicGet(qname);
        assertNotNull("Expected a presence message", r);

        byte[] body = r.getBody();
        if (body != null && body.length > 0) {
            fail("Expected null or empty body in presence message");
        }

        Map<String, Object> h = r.getProps().getHeaders();
        assertNotNull("Expected headers in presence message", h);
        return h;
    }

    public void assertPresence(String qname, String action, String boundQueue, String boundKey) throws IOException {
        Map<String, Object> h = getPresenceHeaders(qname);
        assertEquals(action, getHeaderString(h, "action"));
        assertEquals(xname, getHeaderString(h, "exchange"));
        assertEquals(boundQueue, getHeaderString(h, "queue"));
        assertEquals(boundKey, getHeaderString(h, "key"));
    }

    public void integrateDelta(String qname, Set<String> presence) throws IOException {
        Map<String, Object> h = getPresenceHeaders(qname);
        String action = getHeaderString(h, "action");
        String boundQueue = getHeaderString(h, "queue");
        if ("bind".equals(action)) {
            presence.add(boundQueue);
        } else if ("unbind".equals(action)) {
            presence.remove(boundQueue);
        } else {
            fail("Unexpected binding action");
        }
    }

    public void simpleBind(String whichOtherQueue) throws IOException {
        channel.queueBind(whichOtherQueue, xname, otherKey);
    }

    public void assertBind(String listenQueue, String otherQueue) throws IOException {
        assertPresence(listenQueue, "bind", otherQueue, otherKey);
    }

    public void assertUnbind(String listenQueue, String otherQueue) throws IOException {
        assertPresence(listenQueue, "unbind", otherQueue, otherKey);
    }

    public void testPresenceAndDelete() throws IOException {
        simpleBind(oq1);
        assertBind(lq1, oq1);
        channel.queueDelete(oq1);
        assertUnbind(lq1, oq1);
    }

    public void testPresenceAndUnbind() throws IOException {
        simpleBind(oq1);
        assertBind(lq1, oq1);
        simpleUnbind(oq1);
        assertUnbind(lq1, oq1);
    }

    public void simpleUnbind(String whichOtherQueue) throws IOException {
        channel.queueUnbind(whichOtherQueue, xname, otherKey);
    }

    public void testNoPresenceOnListen() throws IOException {
        listenBind(lq2);
        assertEmpty(lq2); // no non-listeners yet, but most importantly, ...
        assertEmpty(lq1); // ... a new listen queue shouldn't generate presence
    }

    public void testPresenceToBoth() throws IOException {
        listenBind(lq2);
        simpleBind(oq1);
        assertBind(lq1, oq1);
        assertBind(lq2, oq1);
        channel.queueDelete(oq1);
        assertUnbind(lq1, oq1);
        assertUnbind(lq2, oq1);
    }

    public void testInitialReport() throws IOException {
        simpleBind(oq1);
        simpleBind(oq2);
        assertBind(lq1, oq1);
        assertBind(lq1, oq2);

        listenBind(lq2);
        Set<String> present = new HashSet<String>();
        integrateDelta(lq2, present);
        integrateDelta(lq2, present);
        assertEquals(2, present.size());
        assertTrue("contains oq1", present.contains(oq1));
        assertTrue("contains oq2", present.contains(oq2));
    }

    public void testDisabledInitialReport() throws IOException {
        simpleBind(oq1);
        simpleBind(oq2);
        assertBind(lq1, oq1);
        assertBind(lq1, oq2);

        listenBind(lq2, false);
        assertEmpty(lq2);
    }

    public void testOnlyListenersGetPresence() throws IOException {
        simpleBind(oq1);
        simpleBind(oq2);
        assertBind(lq1, oq1);
        assertBind(lq1, oq2);
        assertEmpty(oq1);
        assertEmpty(oq2);
    }

    public void testBothPresenceToBoth() throws IOException {
        simpleBind(oq1);
        listenBind(lq2);
        assertBind(lq2, oq1);
        simpleBind(oq2);

        assertBind(lq1, oq1);
        assertBind(lq1, oq2);
        assertBind(lq2, oq2);

        channel.queueDelete(oq1);
        simpleUnbind(oq2);

        assertUnbind(lq1, oq1);
        assertUnbind(lq2, oq1);
        assertUnbind(lq1, oq2);
        assertUnbind(lq2, oq2);
    }

    public void testNoListenUnbind() throws IOException {
        listenBind(lq2);
        assertEmpty(lq2);
        channel.queueDelete(lq2);
        assertEmpty(lq1); // removal of a listening binding should not cause unbind presence
    }

    public void testMessagesDropped() throws IOException {
        simpleBind(oq1);
        assertBind(lq1, oq1);
        assertEmpty(lq1);
        assertEmpty(oq1);
        basicPublishVolatile(xname, "testing");
        basicPublishVolatile(xname, listenKey);
        basicPublishVolatile(xname, otherKey);
        assertEmpty(lq1);
        assertEmpty(oq1);
    }
}
