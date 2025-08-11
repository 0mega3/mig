/**
 * @license
 * Copyright (c) 2025 Denis Khorkin
 *
 * Licensed under the BSD 3-Clause "New" or "Revised" License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://opensource.org/license/bsd-3-clause
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * MIG Chat Application - client-side chat implementation
 *
 * @namespace MIGChat
 * @property {object} Config - Configuration constants
 * @property {object} DOM - DOM element references
 * @property {object} State - Application state management
 * @property {object} mockServer - Mock server implementation
 * @property {object} mockWs - Mock WebSocket connection
 * @property {object} messageHandlers - Message type handlers
 * @property {object} commands - Chat command registry
 * @property {object} CoreFunctions - Core application functions
 * @property {object} UIRendering - UI rendering functions
 * @property {object} UserFeedback - User notification system
 * @property {object} CommandHandlers - Command handlers
 * @property {object} ConnectionManager - Connection management
 * @property {object} EventHandlers - User interaction handlers
 */

// --- mockServer ---

/**
 * @typedef {Object} ConnectionResult
 * @property {boolean} success - Operation status
 * @property {string} [message] - Optional error message
 */


/**
 * @typedef {Object} JoinResult
 * @property {boolean} success - Operation status
 * @property {string} [message] - Optional error message
 * @property {string[]} users - List of users in room
 * @property {boolean} wasAlreadyInRoom - Was user already in room
 */


/**
 * @typedef {Object} LeaveRoomResult
 * @property {boolean} success - Operation status
 * @property {string} [message] - Optional error message
 * @property {string[]} users - Remaining users in room
 * @property {boolean} roomDeleted - Whether room was deleted
 * @property {boolean} leftCurrent - Whether left current room
 * @property {boolean} disconnected - Whether user fully disconnected
 */


/**
 * @typedef {Object} RoomInfo
 * @property {string} name - Room name with # prefix
 * @property {number} userCount - Number of users in room
 * @property {boolean} isCurrent - Whether this is current room
 */

/**
 * @typedef {Object} UserRoomsResult
 * @property {boolean} success - Operation status
 * @property {RoomInfo[]} rooms - List of rooms with metadata
 */

// --- mockWs ---

/**
 * @typedef {Object} ConnectErrorResponse
 * @property {"connect_error"} type - Error response type identifier
 * @property {string} message - Error description message
 */

/**
 * @typedef {Object} ConnectAckResponse
 * @property {"connect_ack"} type - Success response type identifier
 * @property {string} user - Connected username
 * @property {string} room - Initial room
 * @property {string[]} users - Users in initial room
 * @property {string[]} rooms - All available rooms
 */

/**
 * @typedef {ConnectErrorResponse|ConnectAckResponse} ConnectResponse
 */


/**
 * @typedef {Object} JoinErrorResponse
 * @property {"join_error"} type - Error response type identifier
 * @property {string} message - Error description message
 */

/**
 * @typedef {Object} JoinAckResponse
 * @property {"join_ack"} type - Success response type identifier
 * @property {string} user - Connected username
 * @property {string} room - Room name (with # prefix)
 * @property {string[]} users - Array of users in the room
 * @property {boolean} wasAlreadyInRoom - Flag if user was already in room
 * @property {string[]} allRooms - List of all available rooms
 */

/**
 * @typedef {JoinErrorResponse|JoinAckResponse} JoinResponse
 */


/**
 * @typedef {Object} PartErrorResponse
 * @property {"part_error"} type - Error response type identifier
 * @property {string} message - Error description message
 */

/**
 * @typedef {Object} PartAckResponse
 * @property {"part_ack"} type - Success response type identifier
 * @property {string} user - Username who left
 * @property {string} room - Room name that was left
 * @property {string[]} users - Remaining users in room (empty if room deleted)
 * @property {boolean} leftCurrent - Whether user left their current room
 * @property {boolean} roomDeleted - Whether room was deleted (no users left)
 * @property {boolean} disconnected - Whether user fully disconnected (left all rooms)
 */

/**
 * @typedef {PartErrorResponse|PartAckResponse} PartResponse
 */


/**
 * @typedef {Object} SendErrorResponse
 * @property {"send_error"} type - Error response type identifier
 * @property {string} message - Error description message
 * @property {string} [originalText] - Original message content
 */

/**
 * @typedef {Object} SendAckResponse
 * @property {"send_ack"} type - Success response type identifier
 * @property {string} user - Sender username
 * @property {string} text - Acknowledged message
 */

/**
 * @typedef {SendAckResponse|SendErrorResponse} SendResponse
 */


/**
 * @typedef {Object} QuitErrorResponse
 * @property {"quit_error"} type - Error response type identifier
 * @property {string} message - Error description message
 */

/**
 * @typedef {Object} QuitAckResponse
 * @property {"quit_ack"} type - Success response type identifier
 */

/**
 * @typedef {QuitErrorResponse|QuitAckResponse} QuitResponse
 */


/**
 * @typedef {Object} MyRoomsErrorResponse
 * @property {"myrooms_error"} type - Error response type identifier
 * @property {string} message - Error description message
 */

/**
 * @typedef {Object} MyRoomsAckResponse
 * @property {"myrooms_ack"} type - Success response type identifier
 * @property {boolean} success - Operation status
 * @property {RoomInfo[]} rooms - List of rooms with metadata
 */

/**
 * @typedef {MyRoomsErrorResponse|MyRoomsAckResponse} MyRoomsResponse
 */

$(document).ready(function() {
    /**
     * Core configuration constants
     * @namespace Config
     * @memberof MIGChat
     */
    // Length in UTF-16 units (1-2 per char)
    const USERNAME_LIMITS   = { MAX: 32 };
    const ROOMNAME_LIMITS   = { MIN: 2, MAX: 50 };
    const MESSAGE_LIMITS    = { MAX: 400 };

    /**
     * DOM element references
     * @namespace DOM
     * @memberof MIGChat
     */
    const $messageArea = $('#message-area');
    const $statusLine = $('#status-line');
    const $commandLine = $('#command-line');
    const $usersList = $('#users');

    /**
     * Application state management
     * @namespace State
     * @memberof MIGChat
     */
    let ws = null;
    let currentUser = null;
    let currentRoom = null;

    /**
     * Mock server implementation
     * @namespace mockServer
     * @memberof MIGChat
     * @property {Object.<string, string[]>} rooms - Rooms data where:
     *               key: room name (with '#' prefix),
     *               value: array of connected users
     * @property {Set<string>} users - Connected users
     */
    const mockServer = {
        rooms: { '#general': [] },
        users: new Set(),

        /**
         * Validate and connect user
         * @memberof mockServer
         * @param {string} username - Username to connect
         * @returns {ConnectionResult} Connection result
         */
        connectUser: function(username) {
            if (username.length > USERNAME_LIMITS.MAX) {
                return {
                    success: false,
                    message: `Username too long (max ${USERNAME_LIMITS.MAX} chars)`
                };
            }
            /* Username must be unique across all connections.
             * Note: Mock only checks within current tab. */
            if (this.users.has(username)) {
                return {
                    success: false,
                    message: "Username already taken"
                };
            }
            return { success: true };
        },

        /**
         * Join user to room (creates room if not exists)
         * @memberof mockServer
         * @param {string} user - Username
         * @param {string} room - Room name (with # prefix)
         * @returns {JoinResult} Join operation result
         */
        joinRoom: function(user, room) {
            if (room.length > ROOMNAME_LIMITS.MAX) {
                return {
                    success: false,
                    message: "Room name too long (max 50 chars)",
                    users: []
                };
            }

            if (!this.rooms[room]) this.rooms[room] = [];

            const wasAlreadyInRoom = this.rooms[room].includes(user);
            if (!wasAlreadyInRoom) {
                this.rooms[room].push(user);
            }
            return {
                success: true,
                users: [...this.rooms[room]],
                wasAlreadyInRoom: wasAlreadyInRoom
            };
        },

        /**
         * Remove user from room (deletes room if empty)
         * @memberof mockServer
         * @param {string} user - Username
         * @param {string} room - Room name (with # prefix)
         * @returns {LeaveRoomResult} Leave operation result
         */
        leaveRoom: function(user, room) {
            if (!room) {
                return {
                    success: false,
                    message: "No room specified"    // Status: Inactive
                };
            }

            if (room.length > ROOMNAME_LIMITS.MAX) {
                return {
                    success: false,
                    message: "Room name too long (max 50 chars)"
                };
            }

            if (!this.rooms[room]) {
                return {
                    success: false,
                    message: `Room ${room} not found`
                };
            }

            const leftCurrent = (room === currentRoom);
            let roomDeleted = false;

            this.rooms[room] = this.rooms[room].filter(u => u !== user);
            const usersLeft = this.rooms[room].length;

            if (usersLeft === 0 && room !== '#general') {
                delete this.rooms[room];
                roomDeleted = true;
            }

            const userHasNoRooms = !Object.values(this.rooms)
                .some(roomUsers => roomUsers.includes(user));

            if (userHasNoRooms) {
                this.users.delete(user);
            }

            return {
                success: true,
                users: roomDeleted ? [] : [...this.rooms[room]],
                roomDeleted: roomDeleted,
                leftCurrent: leftCurrent,
                disconnected: userHasNoRooms
            };
        },

        /**
         * Get rooms where user is present
         * @memberof mockServer
         * @param {string} user - Username
         * @returns {UserRoomsResult} User rooms information
         */
        getMyRooms: function(user) {
            const userRooms = Object.keys(this.rooms)
                .filter(room => this.rooms[room].includes(user));

            return {
                success: true,
                rooms: userRooms.map(room => ({
                    name: room,
                    userCount: this.rooms[room].length,
                    isCurrent: (room === currentRoom)
                }))
            };
        }
    };

    /**
     * WebSocket mock implementation with full connection flow handling
     *
     * @namespace mockWs
     * @memberof MIGChat
     * @description Handles all WebSocket communication patterns including:
     *
     * ### Connection Flow:
     * 1. Connect Sequence:
     *    - Client -> connect
     *    - Server -> connect_ack
     *    - On failure: Server -> connect_error
     *
     * ### Normal Quit Flow:
     * 1. Client -> quit
     * 2. Server -> quit_ack
     * 3. Server -> close(1000)
     * 4. Client(onclose)
     *
     * ### Error Cases:
     * - Command Errors (server rejects):
     *   - connect_error: Invalid username
     *   - quit_error: User not connected
     * - Connection Errors:
     *   - onclose: codes 1001-1015 (network issues)
     *   - system_error: internal server errors
     *
     * ### Standard Close Codes:
     * - 1000: Normal closure
     * - 1001: Going away
     * - 1006: Abnormal closure
     * - 4000-4999: Application-specific codes
     *
     * @see {@link https://datatracker.ietf.org/doc/html/rfc6455#section-7.4
     * RFC 6455 Section 7.4}
     */
    const mockWs = {
        /**
         * Handle connect request from client
         * @memberof MIGChat.mockWs
         * @private
         * @param {object} msg - Connection message
         * @param {string} msg.user - Username to connect
         * @returns {ConnectResponse} Connection response
         */
        _handleConnect(msg) {
            console.log(`[MOCK] Processing connect for: ${msg.user}`);

            if (currentUser && currentUser !== msg.user) {
                this._handleQuit({ user: currentUser });
            }

            const response = mockServer.connectUser(msg.user);
            if (!response.success) {
                console.log(`[MOCK] Reject: ${msg.user} (${response.message})`);
                return {
                    type: "connect_error",
                    message: response.message
                };
            }

            mockServer.users.add(msg.user);
            const joinResponse = mockServer.joinRoom(msg.user, '#general');
            console.log(`[MOCK] User ${msg.user} connected to #general`);

            return {
                type: "connect_ack",
                user: msg.user,
                room: '#general',
                // Users list in #general
                users: joinResponse.users,
                // All available rooms
                rooms: Object.keys(mockServer.rooms)
            };
        },

        /**
         * Handle join room request
         * @memberof MIGChat.mockWs
         * @private
         * @param {object} msg - Join message
         * @param {string} msg.user - Username
         * @param {string} msg.room - Room name (with # prefix)
         * @returns {JoinResponse} Join response
         */
        _handleJoin(msg) {
            console.log(`[MOCK] Processing join: ${msg.user} -> ${msg.room}`);

            const response = mockServer.joinRoom(msg.user, msg.room);
            if (!response.success) {
                console.log(`[MOCK] Reject join: ${response.message}`);
                return {
                    type: "join_error",
                    message: response.message
                };
            }

            const action = response.wasAlreadyInRoom ? "switched to" : "joined";
            console.log(`[MOCK] ${msg.user} ${action} ${msg.room}`);

            return {
                type: "join_ack",
                user: msg.user,
                room: msg.room,
                users: response.users,
                wasAlreadyInRoom: response.wasAlreadyInRoom,
                allRooms: Object.keys(mockServer.rooms)
            };
        },

        /**
         * Handle leave room request
         * @memberof MIGChat.mockWs
         * @private
         * @param {object} msg - Leave message
         * @param {string} msg.user - Username
         * @param {string} msg.room - Room name (with # prefix)
         * @returns {PartResponse} Leave operation response
         */
        _handlePart(msg) {
            console.log(`[MOCK] Processing part: ${msg.user} <- ${msg.room}`);

            const response = mockServer.leaveRoom(msg.user, msg.room);
            if (!response.success) {
                console.log(`[MOCK] Part error: ${response.message}`);
                return {
                    type: "part_error",
                    message: response.message
                };
            }

            console.log(`[MOCK] User ${msg.user} left ${msg.room}`);
            if (response.roomDeleted) {
                console.log(`[MOCK] Room ${msg.room} deleted (no users left)`);
            }

            return {
                type: "part_ack",
                user: msg.user,
                room: msg.room,
                users: response.users,
                leftCurrent: response.leftCurrent,
                roomDeleted: response.roomDeleted,
                disconnected: response.disconnected
            };
        },

        /**
         * Handle message sending
         * @memberof MIGChat.mockWs
         * @private
         * @param {object} msg - Message object
         * @param {string} msg.user - Sender username
         * @param {string} msg.room - Target room
         * @param {string} msg.text - Message text
         * @returns {SendResponse} Message operation result
         */
        _handleSend(msg) {
            console.log(`[MOCK] Send: ${msg.user}@${msg.room}: "${msg.text}"`);
            if (msg.text.length > MESSAGE_LIMITS.MAX) {
                return {
                    type: "send_error",
                    message: `Message too long (max ${MESSAGE_LIMITS.MAX} chars)`,
                    originalText: msg.text
                };
            }

            return {
                type: "send_ack",
                user: msg.user,
                text: msg.text
            };
        },

        /**
         * Handle quit request
         * @memberof MIGChat.mockWs
         * @private
         * @param {object} msg - Quit message
         * @param {string} msg.user - Username to disconnect
         * @returns {QuitResponse} Quit operation response
         */
        _handleQuit(msg) {
            console.log(`[MOCK] Processing quit for: ${msg.user}`);

            if (!mockServer.users.has(msg.user)) {
                return {
                    type: "quit_error",
                    message: "User not found"
                };
            }

            // Cleans up server state
            mockServer.users.delete(msg.user);
            for (const room in mockServer.rooms) {
                mockServer.leaveRoom(msg.user, room);
            }

            return { type: "quit_ack" };
            // Connection close will be handled in send()
        },

        /**
         * Handle rooms list request
         * @memberof MIGChat.mockWs
         * @private
         * @param {object} msg - Request message
         * @param {string} msg.user - Username
         * @returns {MyRoomsResponse} Rooms list response
         */
        _handleGetMyRooms(msg) {
            console.log(`[MOCK] Request rooms for: ${msg.user}`);
            const response = mockServer.getMyRooms(msg.user);

            if (!response.success) {
                console.log(`[MOCK] Error getting rooms: ${response.message}`);
            }

            return {
                type: response.success ? "myrooms_ack" : "myrooms_error",
                ...response
            };
        },

        /**
         * Simulate WebSocket connection opening
         * @memberof MIGChat.mockWs
         */
        open() {
            console.log("[MOCK] Connection opened");
            if (this.onopen) {
                setTimeout(() => this.onopen(), 0); // Async call
            }
        },

        /**
         * Mock server-initiated connection close
         * - Triggers onclose event
         * - Uses standard WebSocket close codes
         * @memberof MIGChat.mockWs
         * @param {number} [code=1000] - WebSocket close code
         * @param {string} [reason="Server closed connection"] - Close reason
         */
        close(code = 1000, reason = "Server closed connection") {
            console.log(`[MOCK] Closing with code ${code}: ${reason}`);
            if (this.onclose) {
                this.onclose({
                    wasClean: code === 1000,
                    code: code,
                    reason: reason
                });
            }
        },

        /**
         * Process incoming WebSocket message
         * @memberof MIGChat.mockWs
         * @param {string} data - JSON string message
         * @throws {Error} When message processing fails
         */
        send(data) {
            const msg = JSON.parse(data);
            console.log("[MOCK] WS received:", msg);

            setTimeout(() => {
                if (!this.onmessage) {
                    console.warn("[MOCK] No message handler registered!");
                    return;
                }

                try {
                    let response;
                    switch (msg.type) {
                        case "connect":
                            response = this._handleConnect(msg);
                            break;
                        case "join":
                        case "create":
                            response = this._handleJoin(msg);
                            break;
                        case "part":
                            response = this._handlePart(msg);
                            break;
                        case "send":
                            response = this._handleSend(msg);
                            // Send broadcast messages
                            mockServer.rooms[msg.room]?.forEach(user => {
                                this.onmessage({
                                    data: JSON.stringify(response)
                                });
                            });
                            return;
                        case "quit":
                            response = this._handleQuit(msg);
                            if (response) {
                                // Send acknowledgement (ACK)
                                this.onmessage({
                                    data: JSON.stringify(response)
                                });
                                // Close connection after short delay
                                setTimeout(() => this.close(), 100);
                            }
                            return;
                        case "get_myrooms":
                            response = this._handleGetMyRooms(msg);
                            break;
                        default:
                            console.warn(`[MOCK] Unknown message type: ${msg.type}`);
                            return;
                    }

                    if (response) {
                        this.onmessage({
                            data: JSON.stringify(response)
                        });
                    }
                } catch (error) {
                    console.error("[MOCK] Processing error:", error);
                    this.onmessage({
                        data: JSON.stringify({
                            type: "system_error",
                            message: "Internal server error"
                        })
                    });
                }
            }, 300);
        }
    };

    /**
     * WebSocket event handlers and message processing
     * @namespace WebSocketHandlers
     * @memberof MIGChat
     */

    // --- mockWs.onopen() ---

    /**
     * Handles successful WebSocket connection establishment
     * @memberof MIGChat.WebSocketHandlers
     * @function onopen
     */
    mockWs.onopen = function() {
        console.log("[MOCK] Connection established");
    };

    // --- mockWs.onclose(event) ---

    /**
     * Handles WebSocket connection closure
     * @memberof MIGChat.WebSocketHandlers
     * @function onclose
     * @param {CloseEvent} event - Close event details
     * @property {number} event.code - WebSocket close code
     * @property {string} event.reason - Close reason
     * @property {boolean} event.wasClean - True if clean closure
     */
    mockWs.onclose = function(event) {
        if (event.code !== 1000) {
            console.warn("[MOCK] Abnormal connection close:", {
                code: event.code,
                reason: event.reason,
                wasClean: event.wasClean
            });
        }

        console.log("[MOCK] Connection closed by server", event);

        // Clean up client state
        currentUser = null;
        currentRoom = null;
        ws = null;

        // Generate detailed closure message
        let message = 'Disconnected: ';
        message += event.code === 1000
            ? 'Normal closure'
            : `Error (code ${event.code})`;
        if (event.reason) message += `: ${event.reason}`

        // Update UI
        updateStatus();
        updateRoomsUI();
        updateUsersUI([]);
        showSystemMessage(message);
    };

    // --- Message Handlers ---

    /**
     * Message handlers for different WebSocket message types
     * @namespace MessageHandlers
     * @memberof MIGChat
     */
    const messageHandlers = {
        /**
         * Handle successful connection acknowledgement
         * @memberof MIGChat.MessageHandlers
         * @param {object} data - Response data
         * @param {string} data.user - Connected username
         * @param {string} data.room - Initial room
         * @param {string[]} data.users - Users in initial room
         */
        handleConnectAck(data) {
            currentUser = data.user;
            currentRoom = data.room;
            updateStatus();
            updateRoomsUI();
            updateUsersUI(data.users);
            showSystemMessage(`Connected as ${data.user}`);
        },

        /**
         * Handle connection error
         * @memberof MIGChat.MessageHandlers
         * @param {object} error - Error details
         * @param {string} error.message - Error description
         */
        handleConnectError(error) {
            showErrorInInput(`Connection failed: ${error.message}`);
            console.error('[MOCK] Connection error:', error.message);
        },

        /**
         * Handle successful room join
         * @memberof MIGChat.MessageHandlers
         * @param {object} data - Response data
         * @param {string} data.room - Joined room
         * @param {string[]} data.users - Users in room
         * @param {boolean} data.wasAlreadyInRoom - True if user was already in room
         */
        handleJoinAck(data) {
            currentRoom = data.room;
            updateStatus();
            updateRoomsUI();
            updateUsersUI(data.users);
            showSystemMessage(data.wasAlreadyInRoom
                ? `Switched to ${data.room}`
                : `Joined ${data.room}`);
        },

        /**
         * Handle room join error
         * @memberof MIGChat.MessageHandlers
         * @param {object} error - Error details
         * @param {string} error.message - Error description
         */
        handleJoinError(error) {
            showErrorInInput(`Can't join room: ${error.message}`);
            console.error('[MOCK] Join error:', error.message);
        },

        /**
         * Handle successful room leave acknowledgement
         * @memberof MIGChat.MessageHandlers
         * @param {object} data - Response data
         * @param {boolean} data.leftCurrent - True if left current room
         * @param {boolean} data.disconnected - True if user disconnected completely
         * @param {string} data.room - Room name that was left
         * @param {string[]} [data.users] - Remaining users in room (if not deleted)
         */
        handlePartAck(data) {
            if (data.leftCurrent) currentRoom = null;
            if (data.disconnected) {
                currentUser = null;
                showSystemMessage("Disconnected (no rooms left)");
            } else {
                showSystemMessage(`Left ${data.room}`);
            }
            updateStatus();
            updateRoomsUI();
            updateUsersUI(data.users || []);    // (empty array if undefined)
        },

        /**
         * Handle room leave error
         * @memberof MIGChat.MessageHandlers
         * @param {object} error - Error details
         * @param {string} error.message - Error description
         */
        handlePartError(error) {
            showErrorInInput(`Can't leave room: ${error.message}`);
            console.error('[MOCK] Part error:', error.message);
        },

        /**
         * Handle successful message delivery
         * @memberof MIGChat.MessageHandlers
         * @param {object} data - Message data
         * @param {string} data.user - Sender username
         * @param {string} data.text - Message content
         */
        handleSendAck(data) {
            showUserMessage(data.user, data.text);
        },

        /**
         * Handle message send error
         * @memberof MIGChat.MessageHandlers
         * @param {object} error - Error details
         * @param {string} error.message - Error description
         */
        handleSendError(error) {
            showErrorInInput(`Message not sent: ${error.message}`);
            console.error('[MOCK] Send error:', error.message);
        },

        /**
         * Handle successful quit acknowledgement
         * @memberof MIGChat.MessageHandlers
         * @description Note: Actual cleanup happens in onclose handler
         */
        handleQuitAck() {
            console.log('[MOCK] Quit acknowledged by server');
        },

        /**
         * Handle quit command error
         * - Called when server explicitly rejects quit request
         * - Different from connection close errors
         * @memberof MIGChat.MessageHandlers
         * @param {object} error - Error details
         * @param {string} error.message - Error description
         */
        handleQuitError(error) {
            console.error('[MOCK] Quit error:', error.message);
            showSystemMessage(`Quit failed: ${error.message}`);
            updateStatus(); // Reflect possible partial state changes
        },

        /**
         * Handle successful rooms list response
         * @memberof MIGChat.MessageHandlers
         * @param {object} data - Response data
         * @param {Array<{
         *   name: string,
         *   userCount: number,
         *   isCurrent: boolean
         * }>} data.rooms - List of rooms with metadata
         */
        handleMyRoomsAck(data) {
            if (data.rooms.length === 0) {
                showSystemMessage("Not in any rooms. Use :join #room");
                return;
            }

            const roomList = data.rooms.map(room =>
                `${room.isCurrent ? '> ' : '- '}${room.name} ` +
                `(${room.userCount} user${room.userCount !== 1 ? 's' : ''})` +
                `${room.isCurrent ? ' [CURRENT]' : ''}`
            ).join('\n');

            $messageArea.append(`
                <div class="system rooms-info">
                    <strong>${currentUser}'s rooms (${data.rooms.length}):</strong>
                    <pre>${roomList}</pre>
                </div>
            `);
            $messageArea.scrollTop($messageArea[0].scrollHeight);
        },

        /**
         * Handle rooms list error
         * @memberof MIGChat.MessageHandlers
         * @param {object} error - Error details
         * @param {string} error.message - Error description
         */
        handleMyRoomsError(error) {
            showSystemMessage(`Can't fetch rooms: ${error.message}`);
            console.error('[MOCK] Rooms error:', error.message);
        }
    };

    // --- mockWs.onmessage(event) ---

    /**
     * Main WebSocket message dispatcher
     * @memberof MIGChat.WebSocketHandlers
     * @function onmessage
     * @param {MessageEvent} event - Incoming message event
     * @param {string} event.data - Message data (JSON string)
     */
    mockWs.onmessage = function(event) {
        const data = JSON.parse(event.data);
        console.log('[MOCK] Received:', data.type, data);

        try {
            switch(data.type) {
                case "connect_ack":
                    messageHandlers.handleConnectAck(data);
                    break;
                case "connect_error":
                    messageHandlers.handleConnectError(data);
                    break;

                case "join_ack":
                    messageHandlers.handleJoinAck(data);
                    break;
                case "join_error":
                    messageHandlers.handleJoinError(data);
                    break;
                case "part_ack":
                    messageHandlers.handlePartAck(data);
                    break;
                case "part_error":
                    messageHandlers.handlePartError(data);
                    break;

                case "send_ack":
                    messageHandlers.handleSendAck(data);
                    break;
                case "send_error":
                    messageHandlers.handleSendError(data);
                    break;

                case "quit_ack":
                    messageHandlers.handleQuitAck();
                    break;
                case "quit_error":
                    messageHandlers.handleQuitError(data);
                    break;

                case "myrooms_ack":
                    messageHandlers.handleMyRoomsAck(data);
                    break;
                case "myrooms_error":
                    messageHandlers.handleMyRoomsError(data);
                    break;

                default:
                    console.warn("Unknown message type:", data.type);
                    showSystemMessage(`Server sent unknown message: ${data.type}`);
            }
        } catch (error) {
            console.error('Message handling failed:', error);
            showErrorInInput('Internal client error');
        }
    };

    /**
     * Command registry for chat application (case-independent)
     *
     * @namespace commands
     * @memberof MIGChat
     * @property {object} core - Essential chat commands
     * @property {object} service - Additional utility commands
     * @property {object} aliases - Shortcuts for core commands (first letter)
     */
    const commands = {
        /**
         * Core chat commands
         * @namespace core
         * @memberof MIGChat.commands
         */
        core: {
            /**
             * Connect to chat with username
             * @memberof MIGChat.commands.core
             * @param {string} username - Unique identifier
             */
            connect: {
                args: { min: 1, max: 1 },
                handler: handleConnect
            },

            /**
             * Join existing room or create new
             * @memberof MIGChat.commands.core
             * @param {string} room - Room name starting with #
             */
            join: {
                args: { min: 1, max: 1 },
                handler: handleJoin
            },

            /**
             * Leave current or specified room
             * @memberof MIGChat.commands.core
             * @param {string} [room] - Optional room name
             */
            part: {
                args: { min: 0, max: 1 },
                handler: handleLeave
            },

            /**
             * Send message to current room
             * @memberof MIGChat.commands.core
             * @param {...string} message - Text to send
             */
            send: {
                args: { min: 1, max: Infinity },
                handler: handleSend
            },

            /**
             * Disconnect from chat
             * @memberof MIGChat.commands.core
             */
            quit: {
                args: { min: 0, max: 0 },
                handler: handleQuit
            }
        },

        /**
         * Service commands
         * @namespace service
         * @memberof MIGChat.commands
         */
        service: {
            /**
             * List all joined rooms
             * @memberof MIGChat.commands.service
             */
            rooms: {
                args: { min: 0, max: 0 },
                handler: handleRooms
            }
        },

        /**
         * Command aliases
         * @namespace aliases
         * @memberof MIGChat.commands
         */
        aliases: {
            /** @alias MIGChat.commands.core.connect */
            c: { args: { min: 1, max: 1 }, handler: handleConnect },
            /** @alias MIGChat.commands.core.join */
            j: { args: { min: 1, max: 1 }, handler: handleJoin },
            /** @alias MIGChat.commands.core.part */
            p: { args: { min: 0, max: 1 }, handler: handleLeave },
            /** @alias MIGChat.commands.core.send */
            s: { args: { min: 1, max: Infinity }, handler: handleSend },
            /** @alias MIGChat.commands.core.quit */
            q: { args: { min: 0, max: 0 }, handler: handleQuit }
        }
    };

    /**
     * Core application functionality including command processing and validation
     * @namespace CoreFunctions
     * @memberof MIGChat
     */

    /**
     * Initialize sidebar UI elements and behaviors
     * @memberof MIGChat.CoreFunctions
     * @function initSidebar
     * @returns {void}
     */
    function initSidebar() {
        updateRoomsUI();

        const VSN = "0.1.0";
        const $sidebar = $('#sidebar');
        const $toggle = $('#sidebar-toggle');

        const titleSpan = document.querySelector("#sidebar-header span");
        if (titleSpan) {
            titleSpan.textContent = `${titleSpan.textContent} v${VSN}`;
        }

        $toggle.on('click', function() {
            $sidebar.toggleClass('collapsed');
            $toggle.html($sidebar.hasClass('collapsed') ? '»' : '≡');
        });
    }

    /**
     * Unified command accessor
     * @memberof MIGChat.CoreFunctions
     * @function getCommand
     * @param {string} command - Command name or alias
     * @returns {object|null} Command configuration object or null if not found
     */
    function getCommand(command) {
        return commands.core[command] ||
            commands.service[command] ||
            commands.aliases[command] ||
            null;
    }

    /**
     * Process chat command and validate arguments
     * @memberof MIGChat.CoreFunctions
     * @function processCommand
     * @param {string} command - Command name (e.g. "join", "send")
     * @param {string[]} args - Array of command arguments
     * @throws {Error} When command processing fails
     */
    function processCommand(command, args) {
        if (!command) {
            showErrorInInput('Empty command');
            return;
        }

        const cmd = getCommand(command);
        if (!cmd) {
            showErrorInInput(`Unknown command: ${command}`);
            return;
        }

        if (!validateArgCount(cmd, args)) return;

        const processedArgs = preprocessCommandArgs(command, args);
        if (!processedArgs) return;

        executeCommand(cmd, processedArgs);
    }

    // --- Validation Helpers ---

    /**
     * Check argument count matches command requirements
     * @memberof MIGChat.CoreFunctions
     * @function validateArgCount
     * @param {object} cmd - Command configuration object
     * @param {string[]} args - Provided arguments array
     * @returns {boolean} True if argument count is valid
     */
    function validateArgCount(cmd, args) {
        const isArgCountValid = typeof cmd.args === 'object'
            ? args.length >= cmd.args.min && args.length <= cmd.args.max
            : args.length === cmd.args; // BC for numeric args

        if (!isArgCountValid) {
            showErrorInInput(`Usage: :${cmd.name}${getArgHint(cmd.args)}`);
            return false;
        }
        return true;
    }

    /**
     * Validate room name format
     * @memberof MIGChat.CoreFunctions
     * @function validateRoomArgs
     * @param {string} roomName - Room name to validate (with # prefix)
     * @returns {boolean} True if room name is valid
     */
    function validateRoomArgs(roomName) {
        if (roomName && !roomName.startsWith('#')) {
            showErrorInInput('Room must start with # (e.g., #general)');
            return false;
        }
        if (roomName && roomName.length < ROOMNAME_LIMITS.MIN) {
            showErrorInInput(`Room name too short ` +
                `(min ${ROOMNAME_LIMITS.MIN} chars)`);
            return false;
        }
        return true;
    }

    // --- Processing ---

    /**
     * Pre-process command arguments based on command type
     * @memberof MIGChat.CoreFunctions
     * @function preprocessCommandArgs
     * @param {string} command - Command name being processed
     * @param {string[]} args - Raw arguments array
     * @returns {string|array|null} Processed arguments or null if validation failed
     */
    function preprocessCommandArgs(command, args) {
        switch (command) {
            case 'join':
            case 'create':
            case 'part':
                return validateRoomArgs(args[0]) ? args : null;

            case 'send':
                return processMessageArgs(args);

            default:
                return args;
        }
    }

    /**
     * Process message arguments into single line string
     * @memberof MIGChat.CoreFunctions
     * @function processMessageArgs
     * @param {string[]} args - Message parts array
     * @returns {string|null} Combined message string or null if empty
     */
    function processMessageArgs(args) {
        const message = args.join(' ').replace(/[\r\n]+/g, ' ').trim();
        if (!message) {
            showErrorInInput('Message cannot be empty');
            return null;
        }
        return message;
    }

    // --- Execution ---

    /**
     * Execute validated command with processed arguments
     * @memberof MIGChat.CoreFunctions
     * @function executeCommand
     * @param {object} cmd - Command configuration object
     * @param {string|string[]} processedArgs - Validated arguments
     */
    function executeCommand(cmd, processedArgs) {
        cmd.handler(processedArgs);
        if (!$commandLine.hasClass('error')) {
            $commandLine.val('');
        }
    }

    // --- Utils ---

    /**
     * Generate argument hint for usage errors
     * @memberof MIGChat.CoreFunctions
     * @function getArgHint
     * @private
     * @param {object|number} argsDef - Arguments definition (min/max or count)
     * @returns {string} Usage hint string (e.g. " <arg>", " [arg]")
     */
    function getArgHint(argsDef) {
        if (typeof argsDef === 'object') {
            if (argsDef.min === 1 && argsDef.max === 1) return ' <arg>';
            if (argsDef.min === 0 && argsDef.max === 1) return ' [arg]';
            if (argsDef.max === Infinity) return ' <message...>';
        }
        return argsDef === 1 ? ' <arg>' : '';
    }

    /**
     * User Interface rendering and update functions
     * @namespace UIRendering
     * @memberof MIGChat
     */

    /**
     * Render and update the rooms list in sidebar
     * @memberof MIGChat.UIRendering
     * @function updateRoomsUI
     * @description Updates the rooms list display including:
     * - Room names with user counts
     * - Active room highlighting
     * - Click handlers for room switching
     */
    function updateRoomsUI() {
        const $roomsList = $('#rooms');
        $roomsList.empty();

        for (const room in mockServer.rooms) {
            const userCount = mockServer.rooms[room].length;
            const roomItem = $(`<li>${room} (${userCount})</li>`);

            if (room === currentRoom) {
                roomItem.addClass('active');
            }

            roomItem.on('click', () => {
                if (currentUser && room !== currentRoom) {
                    handleJoin([room]);
                }
            });

            $roomsList.append(roomItem);
        }
    }

    /**
     * Update the users list display in sidebar
     * @memberof MIGChat.UIRendering
     * @function updateUsersUI
     * @param {string[]} users - Array of usernames to display
     * @description Clears and repopulates the users list with current room participants
     */
    function updateUsersUI(users) {
        const $usersList = $('#users');
        $usersList.empty();
        users.forEach(user => {
            $usersList.append($('<li>').text(user));
        });
    }

    /**
     * Update the connection status display
     * @memberof MIGChat.UIRendering
     * @function updateStatus
     * @description Shows current connection state including:
     * - Username
     * - Current room (if any)
     * - Room count (when not in active room)
     * - Connection status text
     * - Visual connection indicator
     */
    function updateStatus() {
        const isConnected = currentUser && currentRoom;
        const parts = [];

        if (currentUser) parts.push(currentUser);

        if (currentRoom) parts.push(currentRoom);

        if (currentUser && !currentRoom) {
            const userRooms = Object.keys(mockServer.rooms)
                .filter(room => mockServer.rooms[room].includes(currentUser));
            const count = userRooms.length;

            if (count > 0) {
                parts.push(`[${count} room${count !== 1 ? 's' : ''}]`);
            }
        }

        parts.push(`Status: ${!currentUser
                ? 'Disconnected'
                : (currentRoom ? 'Connected' : 'Inactive')}`);

        $statusLine.text(parts.join(' | '))
            .toggleClass('connected', isConnected);
    }

    /**
     * User feedback and notification system
     * @namespace UserFeedback
     * @memberof MIGChat
     */

    /**
     * Display system notification message in chat area
     * @memberof MIGChat.UserFeedback
     * @function showSystemMessage
     * @param {string} text - System message text to display
     * @description Appends system message with special styling to message area
     */
    function showSystemMessage(text) {
        $messageArea.append(`<div class="system">${text}</div>`);
    }

    /**
     * Display user message in chat area with sender formatting
     * @memberof MIGChat.UserFeedback
     * @function showUserMessage
     * @param {string} user - Sender username
     * @param {string} text - Message content
     * @description Appends user message with username highlighting
     */
    function showUserMessage(user, text) {
        $messageArea.append(`<div><strong>${user}:</strong> ${text}</div>`);
    }

    /**
     * Display error message in command input field
     * @memberof MIGChat.UserFeedback
     * @function showErrorInInput
     * @param {string} msg - Error message text
     * @description Shows error in input field with special error styling
     */
    function showErrorInInput(msg) {
        $commandLine.val(`Error: ${msg}`).addClass('error');
    }

    /**
     * Command handlers for chat operations
     * @namespace CommandHandlers
     * @memberof MIGChat
     */

    /**
     * Handle user connection request
     * @memberof MIGChat.CommandHandlers
     * @function handleConnect
     * @param {string[]} username - Array containing single username string
     * @description Performs user connection with automatic disconnection of previous user
     * @example
     * // Connect new user
     * handleConnect(["newuser"]);
     */
    function handleConnect([username]) {
        if (currentUser && currentUser !== username) {
            handleQuit();

            // Mock implementation skips quit_ack wait because:
            // 1. mockWs processes quit synchronously
            // 2. Server acknowledgement isn't required for workflow
            // Production code should await quit_ack here
        }

        ws = connect();
        ws.send(JSON.stringify({
            type: "connect",
            user: username
        }));
    }

    /**
     * Handle room join/create request
     * @memberof MIGChat.CommandHandlers
     * @function handleJoin
     * @param {string[]} room - Array containing single room name (e.g. ["#general"])
     * @description Joins existing room or creates new one if doesn't exist
     * @example
     * // Join existing room
     * handleJoin(["#general"]);
     *
     * // Create new room
     * handleJoin(["#newroom"]);
     */
    function handleJoin([room]) {
        if (!validateConnection(ws, currentUser)) return;

        ws.send(JSON.stringify({
            type: "join",
            user: currentUser,
            room: room
        }));
    }

    /**
     * Handle room leave request
     * @memberof MIGChat.CommandHandlers
     * @function handleLeave
     * @param {string[]} args - Empty array or array with room name
     * @description Leaves specified room or current room if not specified
     * @example
     * // Leave current room
     * handleLeave([]);
     *
     * // Leave specific room
     * handleLeave(["#general"]);
     */
    function handleLeave(args) {
        if (!validateConnection(ws, currentUser)) return;

        ws.send(JSON.stringify({
            type: "part",
            user: currentUser,
            // args[0] takes precedence over currentRoom when present
            room: args[0] || currentRoom
        }));
    }

    /**
     * Handle message sending
     * @memberof MIGChat.CommandHandlers
     * @function handleSend
     * @param {string} message - Message text to send
     * @description Sends message to current room with validation
     * @throws {Error} When not connected or no room joined
     * @example
     * // Send regular message
     * handleSend("Hello everyone!");
     */
    function handleSend(message) {
        if (!validateConnection(ws, currentUser)) return;
        if (!currentRoom) {
            showErrorInInput('Join a room first: :join <room>');
            return;
        }

        ws.send(JSON.stringify({
            type: "send",
            user: currentUser,
            room: currentRoom,
            text: message
        }));
    }

    /**
     * Handle user disconnection request
     * @memberof MIGChat.CommandHandlers
     * @function handleQuit
     * @description Initiates graceful disconnection from server
     * @example
     * // Disconnect current user
     * handleQuit();
     */
    function handleQuit() {
        if (!validateConnection(ws, currentUser)) return;

        // Only send quit request, let server initiate close
        ws.send(JSON.stringify({
            type: "quit",
            user: currentUser
        }));
    }

    /**
     * Handle rooms list request
     * @memberof MIGChat.CommandHandlers
     * @function handleRooms
     * @description Requests list of rooms the user has joined
     * @example
     * // Get rooms list
     * handleRooms();
     */
    function handleRooms() {
        if (!validateConnection(ws, currentUser)) return;

        ws.send(JSON.stringify({
            type: "get_myrooms",
            user: currentUser
        }));
    }

    /**
     * WebSocket connection management utilities
     * @namespace ConnectionManager
     * @memberof MIGChat
     */

    /**
     * Establish and manage WebSocket connections
     * @memberof MIGChat.ConnectionManager
     * @function connect
     * @returns {object} Active WebSocket connection instance
     * @description Handles mock WebSocket connection lifecycle:
     * - Initializes connection if not exists
     * - Triggers asynchronous opening process
     * - Returns connection instance
     * @example
     * // Establish connection
     * const connection = connect();
     */
    function connect() {
        if (!ws) {
            ws = mockWs;
            ws.open();
            console.log("[MOCK] Connection initialized");
        }
        return ws;
    }

    /**
     * Validate connection and authentication state
     * @memberof MIGChat.ConnectionManager
     * @function validateConnection
     * @param {object} connection - WebSocket connection to validate
     * @param {string|null} user - Current username if authenticated
     * @returns {boolean} True if connection is valid and user is authenticated
     * @description Verifies preconditions for chat operations:
     * - Active WebSocket connection must exist
     * - User must be authenticated
     * - Shows appropriate error messages if validation fails
     * @example
     * // Check before sending message
     * if (validateConnection(ws, currentUser)) {
     *    // Safe to proceed
     * }
     */
    function validateConnection(connection, user) {
        if (!connection) {
            showErrorInInput("Not connected to server");
            return false;
        }
        if (!user) {
            showErrorInInput("Connect first: :connect <username>");
            return false;
        }
        return true;
    }

    /**
     * Global event handlers for user interactions
     * @namespace EventHandlers
     * @memberof MIGChat
     */

    /**
     * Colon (:) hotkey handler for command mode
     * @memberof MIGChat.EventHandlers
     * @listens keypress
     * @param {KeyboardEvent} e - Keyboard event
     * @description Handles colon keypress to:
     * - Activate command input mode
     * - Preserve existing input
     * - Clear error states
     * - Position cursor appropriately
     */
    $(document).on('keypress', function(e) {
        if (e.key === ':') {
            e.preventDefault(); // Prevents automatic insertion ':'
            if ($commandLine.hasClass('error')) {
                $commandLine.focus().val(':').removeClass('error');
            } else {
                $commandLine.focus().val(':' + $commandLine.val());
            }
            $commandLine[0].setSelectionRange(1, 1);
        }
    });

    /**
     * Command/message submission handler
     * @memberof MIGChat.EventHandlers
     * @listens keypress
     * @param {KeyboardEvent} e - Keyboard event
     * @description Processes Enter key to:
     * - Execute commands (starting with :)
     * - Send regular messages
     * - Clear input after sending
     */
    $commandLine.on('keypress', function(e) {
        if (e.which === 13) { // Enter
            const input = $commandLine.val().trim();

            if (!input) return; // Ignore empty input

            if (input.startsWith(':')) {
                const cmd = input.slice(1);
                const parts = cmd.split(' ');
                const command = parts[0].toLowerCase();
                const args = parts.slice(1);
                //console.log(`DBG: "${command}" with args:`, args);
                processCommand(command, args);
            } else {
                handleSend(input);
                $commandLine.val('');   // Clear input after sending
            }
        }
    });

    /**
     * Special keyboard shortcuts handler
     * @memberof MIGChat.EventHandlers
     * @listens keydown
     * @param {KeyboardEvent} e - Keyboard event
     * @description Handles terminal-style shortcuts:
     * - Ctrl+K: Delete to end of line
     * - Ctrl+U: Clear entire line
     * - Ctrl+A: Move to line start
     * - Ctrl+E: Move to line end
     */
    $commandLine.on('keydown', function(e) {
        const input = $(this);
        const val = input.val();
        const cursorPos = this.selectionStart;

        if (e.ctrlKey && e.key === 'k') {
            e.preventDefault();
            const newVal = val.substring(0, cursorPos);
            input.val(newVal)
                .removeClass('error');
            this.setSelectionRange(cursorPos, cursorPos);
            return;
        }

        if (e.ctrlKey && e.key === 'u') {
            e.preventDefault();
            $commandLine.val('')
                .removeClass('error')
                .attr('placeholder', 'Enter a command...');
        }

        if (e.ctrlKey && e.key === 'a') {
            e.preventDefault();
            this.setSelectionRange(0, 0);
            return;
        }

        if (e.ctrlKey && e.key === 'e') {
            e.preventDefault();
            this.setSelectionRange(val.length, val.length);
            return;
        }
    });

    // Final initialization
    $commandLine.val('').removeClass('error');
    initSidebar();
    updateStatus();
});
