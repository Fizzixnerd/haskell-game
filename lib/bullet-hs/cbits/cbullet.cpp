#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>
#include <BulletDynamics/Character/btKinematicCharacterController.h>
#include <BulletCollision/CollisionDispatch/btGhostObject.h>

#include "cbullet.h"

extern "C" {

  broadphase_interface* new_broadphase_interface() {
    return reinterpret_cast<broadphase_interface*>(new btDbvtBroadphase());
  }
  
  void free_broadphase_interface(broadphase_interface* broadphase) {
    delete reinterpret_cast<btDbvtBroadphase*>(broadphase);
  }
  
  overlapping_pair_cache* get_overlapping_pair_cache
  (broadphase_interface* broadphase) {
    return reinterpret_cast<overlapping_pair_cache*>
      (reinterpret_cast<btBroadphaseInterface*>(broadphase)->
       getOverlappingPairCache());
  }

  void set_internal_ghost_pair_callback(overlapping_pair_cache* cache,
					ghost_pair_callback* callback) {
    reinterpret_cast<btOverlappingPairCache*>(cache)->
      setInternalGhostPairCallback(reinterpret_cast<btGhostPairCallback*>(callback));
  }

  ghost_pair_callback* new_ghost_pair_callback() {
    return reinterpret_cast<ghost_pair_callback*>(new btGhostPairCallback());
  }

  void free_ghost_pair_callback(ghost_pair_callback* callback) {
    delete reinterpret_cast<btGhostPairCallback*>(callback);
  }

  collision_configuration* new_default_collision_configuration() {
    return reinterpret_cast<collision_configuration*>
      (new btDefaultCollisionConfiguration());
  }

  void free_collision_configuration(collision_configuration* collision_configuration) {
    delete reinterpret_cast<btCollisionConfiguration*>(collision_configuration);
  }

  collision_dispatcher* new_collision_dispatcher
  (collision_configuration* collision_configuration) {
    return reinterpret_cast<collision_dispatcher*>
      (new btCollisionDispatcher
       (reinterpret_cast<btCollisionConfiguration*>
	(collision_configuration)));
  }

  void free_collision_dispatcher(collision_dispatcher* dispatcher) {
    delete reinterpret_cast<btCollisionDispatcher*>(dispatcher);
  }

  constraint_solver* new_sequential_impulse_constraint_solver() {
    return reinterpret_cast<constraint_solver*>
      (new btSequentialImpulseConstraintSolver());
  }
  
  void free_constraint_solver(constraint_solver* solver) {
    delete reinterpret_cast<btConstraintSolver*>(solver);
  }

  // btCollisionWorld::ClosestRayResultCallback
  closest_ray_result_callback* new_closest_ray_result_callback(scalar fromx,
							       scalar fromy,
							       scalar fromz,
							       scalar tox,
							       scalar toy,
							       scalar toz) {
    return reinterpret_cast<closest_ray_result_callback*>
      (new btCollisionWorld::ClosestRayResultCallback
       (btVector3(fromx, fromy, fromz),
	btVector3(tox, toy, toz)));
  }

  void free_closest_ray_result_callback(closest_ray_result_callback* callback) {
    delete reinterpret_cast<btCollisionWorld::ClosestRayResultCallback*>(callback);
  }

  // btCollisionWorld::RayResultCallback
  int rrc_has_hit(ray_result_callback* callback) {
    return reinterpret_cast<btCollisionWorld::ClosestRayResultCallback*>
      (callback)->hasHit();
  }
  
  const collision_object* rrc_get_hit(ray_result_callback* callback) {
    return reinterpret_cast<const collision_object*>
      (reinterpret_cast<btCollisionWorld::ClosestRayResultCallback*>
       (callback)->m_collisionObject);
  }

  // btCollsionWorld
  void ray_test(collision_world* world,
		scalar fromx, scalar fromy, scalar fromz, scalar tox, scalar toy,
		scalar toz, ray_result_callback* callback) {
    btCollisionWorld* theWorld = reinterpret_cast<btCollisionWorld*>(world);
    btVector3 fromVector = btVector3(fromx, fromy, fromz);
    btVector3 toVector = btVector3(tox, toy, toz);
    btCollisionWorld::ClosestRayResultCallback* theCallback = reinterpret_cast<btCollisionWorld::ClosestRayResultCallback*>(callback);
    theWorld->rayTest(fromVector, toVector, *theCallback);
  }

  // btCollisionWorld::ContactResultCallback
  //int crc_has_result(contact_result_callback* callback);

  // btDiscreteDynamicsWorld
  dynamics_world* new_discrete_dynamics_world
  (collision_dispatcher* dispatcher,
   broadphase_interface* broadphase,
   constraint_solver* solver,
   collision_configuration* collision_configuration) {
    return reinterpret_cast<dynamics_world*>(new btDiscreteDynamicsWorld
      (reinterpret_cast<btDispatcher*>(dispatcher),
       reinterpret_cast<btBroadphaseInterface*>(broadphase),
       reinterpret_cast<btConstraintSolver*>(solver),
       reinterpret_cast<btCollisionConfiguration*>(collision_configuration)));
  }

  void free_dynamics_world(dynamics_world* world) {
    delete reinterpret_cast<btDynamicsWorld*>(world);
  }

  void dw_set_gravity(dynamics_world* world, scalar x, scalar y, scalar z) {
    reinterpret_cast<btDynamicsWorld*>(world)->setGravity(btVector3(x, y, z));
  }

  int step_simulation(dynamics_world* world, scalar time_step, int* max_sub_steps, scalar* fixed_time_step) {
    int mss = max_sub_steps ? *max_sub_steps : 1;
    scalar fts = fixed_time_step ? *fixed_time_step : (1.0f / 60.0f);
    return reinterpret_cast<btDynamicsWorld*>(world)->
      stepSimulation(time_step, mss, fts);
  }
  
  void add_rigid_body(dynamics_world* world, rigid_body* rigid_body) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      addRigidBody(reinterpret_cast<btRigidBody*>(rigid_body));
  }

  int get_num_collision_objects(dynamics_world* world) {
    return reinterpret_cast<btDynamicsWorld*>(world)->getNumCollisionObjects();
  }

  collision_object* get_collision_object(dynamics_world* world, int idx) {
    return reinterpret_cast<collision_object*>
      (reinterpret_cast<btDynamicsWorld*>(world)->
       getCollisionObjectArray()[idx]);
  }

  void add_collision_object(dynamics_world* world, collision_object* obj) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      addCollisionObject(reinterpret_cast<btCollisionObject*>(obj));
  }

  void remove_collision_object(dynamics_world* world, collision_object* obj) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      removeCollisionObject(reinterpret_cast<btCollisionObject*>(obj));
  }

  void add_action(dynamics_world* world, action_interface* action) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      addAction(reinterpret_cast<btActionInterface*>(action));
  }

  void remove_action(dynamics_world* world, action_interface* action) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      removeAction(reinterpret_cast<btActionInterface*>(action));
  }

  int get_num_constraints(dynamics_world* world) {
    return reinterpret_cast<btDynamicsWorld*>(world)->getNumConstraints();
  }
  
  typed_constraint* get_constraint(dynamics_world* world, int idx) {
    return reinterpret_cast<typed_constraint*>
      (reinterpret_cast<btDynamicsWorld*>(world)->getConstraint(idx));
  }

  void add_constraint(dynamics_world* world, typed_constraint* constraint) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      addConstraint(reinterpret_cast<btTypedConstraint*>(constraint));
  }

  void remove_constraint(dynamics_world* world, typed_constraint* constraint) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      removeConstraint(reinterpret_cast<btTypedConstraint*>(constraint));
  }

  void dw_serialize(dynamics_world* world, serializer* serializer) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      serialize(reinterpret_cast<btSerializer*>(serializer));
  }

  // btCollisionObject
  collision_object* new_collision_object() {
    return reinterpret_cast<collision_object*>(new btCollisionObject());
  }
  
  void free_collision_object(collision_object* obj) {
    delete reinterpret_cast<btCollisionObject*>(obj);
  }

  transform* co_allocate_world_transform(collision_object* obj) {
      btTransform trans = reinterpret_cast<btCollisionObject*>(obj)->
	getWorldTransform();
      btTransform* pTrans = new btTransform(trans);
      return reinterpret_cast<transform*>(pTrans);
  }

  void co_set_world_transform(collision_object* obj, transform* transform) {
    reinterpret_cast<btCollisionObject*>(obj)->
      setWorldTransform(*reinterpret_cast<btTransform*>(transform));
  }

  int is_static_object(collision_object* obj) {
    return reinterpret_cast<btCollisionObject*>(obj)->
      isStaticObject();
  }

  int is_kinematic_object(collision_object* obj) {
    return reinterpret_cast<btCollisionObject*>(obj)->
      isKinematicObject();
  }

  int is_static_or_kinematic_object(collision_object* obj) {
    return reinterpret_cast<btCollisionObject*>(obj)->
      isStaticOrKinematicObject();
  }

  int has_contact_response(collision_object* obj) {
    return reinterpret_cast<btCollisionObject*>(obj)->
      hasContactResponse();
  }

  collision_shape* get_collision_shape(collision_object* obj) {
    return reinterpret_cast<collision_shape*>
      (reinterpret_cast<btCollisionObject*>(obj)->
       getCollisionShape());
  }

  void set_collision_shape(collision_object* obj, collision_shape* shape) {
    reinterpret_cast<btCollisionObject*>(obj)->
      setCollisionShape(reinterpret_cast<btCollisionShape*>(shape));
  }

  void set_activation_state(collision_object* obj, int new_state) {
    reinterpret_cast<btCollisionObject*>(obj)->
      setActivationState(new_state);
  }

  void get_interpolation_linear_velocity(collision_object* obj, scalar* x, scalar* y,
					 scalar* z) {
    btVector3 v = reinterpret_cast<btCollisionObject*>(obj)->
      getInterpolationLinearVelocity();
    *x = v[0];
    *y = v[1];
    *z = v[2];
  }

  void set_interpolation_linear_velocity(collision_object* obj, scalar x, scalar y,
					 scalar z) {
    reinterpret_cast<btCollisionObject*>(obj)->
      setInterpolationLinearVelocity(btVector3(x, y, z));
  }

  void set_user_index(collision_object* obj, int n) {
    reinterpret_cast<btCollisionObject*>(obj)->
      setUserIndex(n);
  }

  int get_user_index(collision_object* obj) {
    return reinterpret_cast<btCollisionObject*>(obj)->
      getUserIndex();
  }

  // btMotionState
  motion_state* new_default_motion_state(transform* transform) {
    return reinterpret_cast<motion_state*>
      (new btDefaultMotionState(*reinterpret_cast<btTransform*>(transform)));
  }

  void free_motion_state(motion_state* motion_state) {
    delete reinterpret_cast<btMotionState*>(motion_state);
  }

  transform* ms_allocate_world_transform(motion_state* motion_state) {
    btTransform* trans = new btTransform;
    reinterpret_cast<btMotionState*>(motion_state)->getWorldTransform(*trans);
    return reinterpret_cast<transform*>(trans);
  }

  void ms_set_world_transform(motion_state* motion_state, transform* transform) {
    reinterpret_cast<btMotionState*>(motion_state)->
      setWorldTransform(*reinterpret_cast<btTransform*>(transform));
  }

  // btRigidBodyConstructioninfo
  rigid_body_construction_info* new_rigid_body_construction_info
  (scalar mass,
   motion_state* motion_state,
   collision_shape* collision_shape,
   scalar inertia_x,
   scalar inertia_y,
   scalar inertia_z) {
    return reinterpret_cast<rigid_body_construction_info*>
      (new btRigidBody::btRigidBodyConstructionInfo
       (mass,
	reinterpret_cast<btMotionState*>(motion_state),
	reinterpret_cast<btCollisionShape*>(collision_shape),
	btVector3(inertia_x, inertia_y, inertia_z)));
  }

  void free_rigid_body_construction_info(rigid_body_construction_info* rbci) {
    delete reinterpret_cast<btRigidBody::btRigidBodyConstructionInfo*>(rbci);
  }

  // btRigidbody
  rigid_body* new_rigid_body(rigid_body_construction_info* rbci) {
    btRigidBody* rb = new btRigidBody
      (*reinterpret_cast<btRigidBody::btRigidBodyConstructionInfo*>(rbci));
    return reinterpret_cast<rigid_body*>(rb);
  }

  void free_rigid_body(rigid_body* rigid_body) {
    delete reinterpret_cast<btRigidBody*>(rigid_body);
  }

  motion_state* rb_get_motion_state(rigid_body* rigid_body) {
    return reinterpret_cast<motion_state*>
      (reinterpret_cast<btRigidBody*>(rigid_body)->
       getMotionState());
  }

  void rb_set_gravity(rigid_body* body, scalar x, scalar y, scalar z) {
    reinterpret_cast<btRigidBody*>(body)->setGravity(btVector3(x, y, z));
  }

  void rb_get_gravity(rigid_body* body, scalar* x, scalar* y, scalar* z) {
    btVector3 g = reinterpret_cast<btRigidBody*>(body)->getGravity();
    *x = g[0];
    *y = g[1];
    *z = g[2];
  }

  void rb_set_linear_velocity(rigid_body* body, scalar x, scalar y, scalar z) {
    reinterpret_cast<btRigidBody*>(body)->setLinearVelocity(btVector3(x, y, z));
  }

  void rb_get_linear_velocity(rigid_body* body, scalar* x, scalar* y, scalar* z) {
    btVector3 g = reinterpret_cast<btRigidBody*>(body)->getLinearVelocity();
    *x = g[0];
    *y = g[1];
    *z = g[2];
  }

  void get_total_force(rigid_body* body, scalar* x, scalar* y, scalar* z) {
    btVector3 F = reinterpret_cast<btRigidBody*>(body)->getTotalForce();
    *x = F[0];
    *y = F[1];
    *z = F[2];
  }

  void get_total_torque(rigid_body* body, scalar* x, scalar* y, scalar* z) {
    btVector3 T = reinterpret_cast<btRigidBody*>(body)->getTotalTorque();
    *x = T[0];
    *y = T[1];
    *z = T[2];
  }

  void apply_force(rigid_body* body,
		   scalar x, scalar y, scalar z,
		   scalar rel_x, scalar rel_y, scalar rel_z) {
    reinterpret_cast<btRigidBody*>(body)->
      applyForce(btVector3(x, y, z), btVector3(rel_x, rel_y, rel_z));
  }

  void apply_torque(rigid_body* body, scalar x, scalar y, scalar z){
    reinterpret_cast<btRigidBody*>(body)->applyTorque(btVector3(x, y, z));
  }

  void clear_forces(rigid_body* body) {
    reinterpret_cast<btRigidBody*>(body)->clearForces();
  }

  transform* allocate_center_of_mass_transform(rigid_body* rigid_body) {
    return reinterpret_cast<transform*>
      (new btTransform
       (reinterpret_cast<btRigidBody*>(rigid_body)->
	getCenterOfMassTransform()));
  }

  // btCollisionShape
  void free_collision_shape(collision_shape* shape) {
    delete reinterpret_cast<btCollisionShape*>(shape);
  }

  void calculate_local_inertia(collision_shape* shape,
			       scalar mass,
			       scalar* x_out,
			       scalar* y_out,
			       scalar* z_out) {
    btVector3 inertia;
    reinterpret_cast<btCollisionShape*>(shape)->
      calculateLocalInertia(mass, inertia);
    *x_out = inertia[0];
    *y_out = inertia[1];
    *z_out = inertia[2];
  }

  void get_bounding_sphere(collision_shape* shape,
			   scalar* x,
			   scalar* y,
			   scalar* z,
			   scalar* r) {
    btVector3 loc;
    btScalar rad;
    reinterpret_cast<btCollisionShape*>(shape)->getBoundingSphere(loc, rad);
    *x = loc[0];
    *y = loc[1];
    *z = loc[2];
    *r = rad;
  }

  int is_convex(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isConvex();
  }

  int is_polyhedral(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isPolyhedral();
  }
  int is_non_moving(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isNonMoving();
  }

  int is_concave(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isConcave();
  }

  int is_compound(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isCompound();
  }

  int is_soft_body(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isSoftBody();
  }

  int is_infinite(collision_shape* shape) {
    reinterpret_cast<btCollisionShape*>(shape)->isInfinite();
  }

  // btStaticPlaneShape
  static_plane_shape* new_static_plane_shape(scalar x,
					     scalar y,
					     scalar z,
					     scalar plane_constant) {
    return reinterpret_cast<static_plane_shape*>(new btStaticPlaneShape(btVector3(x, y ,z), plane_constant));
  }
  
  void free_static_plane_shape(static_plane_shape* static_plane_shape) {
    delete reinterpret_cast<btStaticPlaneShape*>(static_plane_shape);
  }

  // btCapsuleShape
  capsule_shape* new_capsule_shape(scalar radius, scalar height) {
    return reinterpret_cast<capsule_shape*>(new btCapsuleShape(radius, height));
  }

  void free_capsule_shape(capsule_shape* capsule_shape) {
    delete reinterpret_cast<btCapsuleShape*>(capsule_shape);
  }

  // btSphereShape
  sphere_shape* new_sphere_shape(scalar radius) {
    return reinterpret_cast<sphere_shape*>(new btSphereShape(radius));
  }

  void free_sphere_shape(sphere_shape* sphere_shape) {
    delete reinterpret_cast<btSphereShape*>(sphere_shape);
  }

  //btBoxShape
  box_shape* new_box_shape(scalar half_x, scalar half_y, scalar half_z) {
    return reinterpret_cast<box_shape*>(new btBoxShape(btVector3(half_x, half_y, half_z)));
  }

  void free_box_shape(box_shape* box_shape) {
    delete reinterpret_cast<btBoxShape*>(box_shape);
  }

  // btTransform
  transform* new_transform(scalar r, // quaternion
			   scalar i,
			   scalar j,
			   scalar k,
			   scalar x, // vector
			   scalar y,
			   scalar z) {
    return reinterpret_cast<transform*>
      (new btTransform(btQuaternion(r, i, j, k), btVector3(x, y, z)));
  }

  void free_transform(transform* transform) {
    delete reinterpret_cast<btTransform*>(transform);
  }

  void get_origin(transform* transform, scalar* x_out, scalar* y_out, scalar* z_out) {
    btVector3 vec = reinterpret_cast<btTransform*>(transform)->getOrigin();
    *x_out = vec[0];
    *y_out = vec[1];
    *z_out = vec[2];
  }

  void set_origin(transform* transform, scalar x, scalar y, scalar z) {
    reinterpret_cast<btTransform*>(transform)->setOrigin(btVector3(x, y, z));
  }

  void get_rotation(transform* transform, scalar* i_out, scalar* j_out, scalar* k_out, scalar* r_out) {
    btQuaternion quat = reinterpret_cast<btTransform*>(transform)->getRotation();
    *i_out = quat[0];
    *j_out = quat[1];
    *k_out = quat[2];
    *r_out = quat[3];
  }

  void set_rotation(transform* transform, scalar i, scalar j, scalar k, scalar r) {
    reinterpret_cast<btTransform*>(transform)->setRotation(btQuaternion(i, j, k, r));
  }

  void set_identity(transform* transform) {
    reinterpret_cast<btTransform*>(transform)->setIdentity();
  }

  void get_opengl_matrix(transform* transform, scalar* out) {
    reinterpret_cast<btTransform*>(transform)->getOpenGLMatrix(out);
  }

  // btTypedConstraint
  void free_typed_constraint(typed_constraint* constraint) {
    delete reinterpret_cast<btTypedConstraint*>(constraint);
  }

  // btPoint2PointConstraint
  point2point_constraint* new_point2point_constraint(rigid_body* rigid_body,
					       scalar pivot_x,
					       scalar pivot_y,
					       scalar pivot_z) {
    return reinterpret_cast<point2point_constraint*>
      (new btPoint2PointConstraint
       (*reinterpret_cast<btRigidBody*>(rigid_body),
	btVector3(pivot_x, pivot_y, pivot_z)));
  }

  void free_point2point_constraint(point2point_constraint* p2p) {
    delete reinterpret_cast<btPoint2PointConstraint*>(p2p);
  }

  // btDefaultSerializer
  serializer* new_default_serializer() {
    return reinterpret_cast<serializer*>(new btDefaultSerializer());
  }
  
  void free_serializer(serializer* serializer) {
    delete reinterpret_cast<btSerializer*>(serializer);
  }

  const unsigned char* get_buffer_pointer(serializer* serializer) {
    return reinterpret_cast<btSerializer*>(serializer)->getBufferPointer();
  }

  int get_current_buffer_size(serializer* serializer) {
    return reinterpret_cast<btSerializer*>(serializer)->getCurrentBufferSize();
  }

  // btPairCachingGhostObject
  pair_caching_ghost_object* new_pair_caching_ghost_object() {
    return reinterpret_cast<pair_caching_ghost_object*>
      (new btPairCachingGhostObject());
  }

  void free_pair_caching_ghost_object(pair_caching_ghost_object* ghost_object) {
    delete reinterpret_cast<btPairCachingGhostObject*>(ghost_object);
  }

  // btKinematicCharacterController
  kinematic_character_controller* new_kinematic_character_controller
  (pair_caching_ghost_object* ghost_object,
   convex_shape* convex_shape,
   scalar step_height) {
    return reinterpret_cast<kinematic_character_controller*>
      (new btKinematicCharacterController
       (reinterpret_cast<btPairCachingGhostObject*>(ghost_object),
	reinterpret_cast<btConvexShape*>(convex_shape),
	step_height));
  }

  void free_kinematic_character_controller(kinematic_character_controller* kcc) {
    delete reinterpret_cast<btKinematicCharacterController*>(kcc);
  }

  void set_up(kinematic_character_controller* kcc, scalar x, scalar y, scalar z) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setUp(btVector3(x, y, z));
  }

  void get_up(kinematic_character_controller* kcc, scalar* x, scalar* y, scalar* z) {
    btVector3 up = reinterpret_cast<btKinematicCharacterController*>(kcc)->getUp();
    *x = up[0];
    *y = up[1];
    *z = up[2];
  }

  void kcc_set_angular_velocity(kinematic_character_controller* kcc,
			    scalar ang1,
			    scalar ang2,
			    scalar ang3) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setAngularVelocity(btVector3(ang1, ang2, ang3));
  }

  void kcc_get_angular_velocity(kinematic_character_controller* kcc,
			    scalar* ang1,
			    scalar* ang2,
			    scalar* ang3) {
    btVector3 omega = reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getAngularVelocity();
    *ang1 = omega[0];
    *ang2 = omega[1];
    *ang3 = omega[2];
  }

  void kcc_set_linear_velocity(kinematic_character_controller* kcc,
			   scalar vx,
			   scalar vy,
			   scalar vz) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setLinearVelocity(btVector3(vx, vy, vz));
  }

  void kcc_get_linear_velocity(kinematic_character_controller* kcc,
			   scalar* vx,
			   scalar* vy,
			   scalar* vz) {
    btVector3 v = reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getLinearVelocity();
    *vx = v[0];
    *vy = v[1];
    *vz = v[2];
  }

  void set_linear_damping(kinematic_character_controller* kcc, scalar d) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setLinearDamping(d);
  }
  
  scalar get_linear_damping(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getLinearDamping();
  }

  void set_angular_damping(kinematic_character_controller* kcc, scalar d) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setAngularDamping(d);
  }
  
  scalar get_angular_damping(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getAngularDamping();
  }

  void reset(kinematic_character_controller* kcc, collision_world* collision_world) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      reset(reinterpret_cast<btCollisionWorld*>(collision_world));
  }

  void warp(kinematic_character_controller* kcc, scalar x, scalar y, scalar z) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      warp(btVector3(x, y, z));
  }
  
  void set_step_height(kinematic_character_controller* kcc, scalar step_height) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setStepHeight(step_height);
  }

  scalar get_step_height(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getStepHeight();
  }

  void set_fall_speed(kinematic_character_controller* kcc, scalar fall_speed) {
  reinterpret_cast<btKinematicCharacterController*>(kcc)->
    setFallSpeed(fall_speed);
  }

  scalar get_fall_speed(kinematic_character_controller* kcc) {
  return reinterpret_cast<btKinematicCharacterController*>(kcc)->
    getFallSpeed();
  }

  void set_jump_speed(kinematic_character_controller* kcc, scalar jump_speed) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setJumpSpeed(jump_speed);
  }

  scalar get_jump_speed(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getJumpSpeed();
  }

  void set_max_jump_height(kinematic_character_controller* kcc,
			   scalar max_jump_height) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setMaxJumpHeight(max_jump_height);
  }

  int can_jump(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      canJump();
  }

  void jump(kinematic_character_controller* kcc) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      jump();
  }

  void apply_impulse(kinematic_character_controller* kcc,
		     scalar ix,
		     scalar iy,
		     scalar iz) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      applyImpulse(btVector3(ix, iy, iz));
  }

  void kcc_set_gravity(kinematic_character_controller* kcc,
		       scalar gx,
		       scalar gy,
		       scalar gz) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setGravity(btVector3(gx, gy, gz));
  }

  void get_gravity(kinematic_character_controller* kcc,
		   scalar* gx,
		   scalar* gy,
		   scalar* gz) {
    btVector3 g = reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getGravity();
    *gx = g[0];
    *gy = g[1];
    *gz = g[2];
  }
    
  void set_max_slope(kinematic_character_controller* kcc, scalar slope_radians) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setMaxSlope(slope_radians);
  }

  scalar get_max_slope(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getMaxSlope();
  }

  void set_max_penetration_depth(kinematic_character_controller* kcc, scalar d) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setMaxPenetrationDepth(d);
  }

  scalar get_max_penetration_depth(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      getMaxPenetrationDepth();
  }

  pair_caching_ghost_object* get_ghost_object(kinematic_character_controller* kcc) {
    return reinterpret_cast<pair_caching_ghost_object*>
      (reinterpret_cast<btKinematicCharacterController*>(kcc)->
       getGhostObject());
  }

  void set_use_ghost_sweep_test(kinematic_character_controller* kcc,
				int bool_use_ghost_object_sweep_test) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setUseGhostSweepTest(bool_use_ghost_object_sweep_test);
  }

  int on_ground(kinematic_character_controller* kcc) {
    return reinterpret_cast<btKinematicCharacterController*>(kcc)->
      onGround();
  }

  void set_up_interpolate(kinematic_character_controller* kcc, int bool_value) {
    reinterpret_cast<btKinematicCharacterController*>(kcc)->
      setUpInterpolate(bool_value);
  }

}
